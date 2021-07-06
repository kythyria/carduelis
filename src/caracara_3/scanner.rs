//! Scanner that converts from indentation to delimiters.

use lazy_static::lazy_static;
use regex::Regex;
use super::{Limits, ParseError};

/// Result of a scan
pub struct Scanned {
    pub delimited_text: String,
    pub lines: Vec<LineInfo>
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LineInfo {
    /// Byte in delimited_text where this line starts
    pub scan_start: u32,

    /// Input byte where this line starts
    pub raw_line_start: u32,

    /// Input byte where the whitespace that provoked an INDENT character starts
    pub raw_indent_start: u32,

    /// Input byte where the start of the actual data starts
    pub raw_data_start: u32,

    /// Input byte after the end of the line's actual content (ie, where the newline or eof starts)
    pub raw_data_end: u32,
}

pub const NEWLINE: char = '\n';
pub const INDENT: char = '\u{FDD0}';
pub const OUTDENT: char = '\u{FDD1}';

lazy_static! {
    static ref LINE_RE: Regex = Regex::new(r"(?P<indent>[\p{White_Space}&&[^\r\n\u{000C}\u{000B}\u{2028}\u{2029}\u{0085}]]*)(?P<data>.*?)(?P<eol>\r\n|[\r\n\u{000C}\u{000B}\u{2028}\u{2029}\u{0085}]|$)").unwrap();
}

pub fn scan_string(input: &str, max_indents: usize) -> Result<Scanned, ParseError> {
    if input.len() >= u32::MAX as usize { return Err(ParseError::InputTooLong) }

    let mut delimited_text = String::with_capacity(input.len());
    let mut lines = Vec::<LineInfo>::with_capacity(input.len()/10);

    let mut indents = vec![""];

    for (curr_line, line_cap) in LINE_RE.captures_iter(input).enumerate() {
        let indent_m = line_cap.name("indent").unwrap();
        let data_m = line_cap.name("data").unwrap();

        let scan_start = delimited_text.len() as u32;

        // indents stores consecutive slices, eg if you used `\t\t\t` to mean indent three times,
        // we'll store vec!["\t", "\t", "\t"]
        //
        // so try and peel off one slice at a time from the front of indent_m.
        // if we remove all of them leaving nothing, then there's no change.
        // if we run out of slices and there's still indent_m left, it's an indent
        //     so add the rest of indent_m to indents and emit an INDENT
        // if we run out of indent_m and there's slices left, it's an outdent,
        //     so emit as many OUTDENTs as there are slices left and pop the slices
        // if a slice can't be stripped off but there's still indent_m left, it's a misdent
        //     so emit OUTDENTs and pop the slices
        //     and emit a MISDENT covering the remaining indent_m
        //     a later stage will probably be confused, but oh well.

        let mut indent_start = data_m.start();

        if data_m.as_str().len() > 0 {
            let remaining_indent = indent_m.as_str();
            for (i, piece) in indents.iter().enumerate() {
                if let Some(new_remain) = remaining_indent.strip_prefix(piece) {
                    if remaining_indent.len() == 0 && i == indents.len()-1 {
                        // no change, yay
                        break;
                    }
                    else if remaining_indent.len() > 0 && i == indents.len()-1 {
                        // indent
                        indents.push(new_remain);
                        indent_start = (new_remain.as_ptr() as usize) - (input.as_ptr() as usize);
                        delimited_text.push(INDENT);

                        if indents.len() > max_indents {
                            return Err(ParseError::TooManyIndents(curr_line))
                        }
                        break;
                    }
                    else if remaining_indent.len() == 0 && i < indents.len()-1 {
                        // outdent
                        let to_pop = (indents.len() - 1) - i;
                        for _ in 0..to_pop {
                            indents.pop();
                            delimited_text.push(OUTDENT);
                        }
                        break;
                    }
                    else {
                        // don't know yet
                        continue;
                    }
                }
                else {
                    return Err(ParseError::BrokenIndent {
                        byte: (remaining_indent.as_ptr() as usize) - (input.as_ptr() as usize),
                        line: curr_line
                    })
                }
            }
        }

        delimited_text.extend(data_m.as_str().chars().map(filter_nonchars));
        delimited_text.push(NEWLINE);
        lines.push(LineInfo {
            scan_start,
            raw_line_start: indent_m.start() as u32,
            raw_indent_start: indent_start as u32,
            raw_data_start: data_m.start() as u32,
            raw_data_end: data_m.end() as u32
        })
    }

    Ok(Scanned {
        delimited_text, lines
    })
}

fn filter_nonchars(c: char) -> char {
    let ci = c as u32 & 0xFFFF;
    if ci == 0xFFFF { '\u{FFFD}' }
    else if ci == 0xFFFE { '\u{FFFD}' }
    else if ('\u{FDD0}' ..= '\u{FDEF}').contains(&c) { '\u{FFFD}' }
    else { c }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn line_re(testitem: &str, expect: Vec<(&str, &str, &str)>) {
        let test_items: Vec<_> = LINE_RE.captures_iter(testitem).map(|m| (
            m.name("indent").unwrap().as_str(),
            m.name("data").unwrap().as_str(),
            m.name("eol").unwrap().as_str()
        )).collect();

        assert_eq!(test_items, expect);
    }

    #[test] fn line_re_spacing() {
        line_re("a\n  b \nc c\r\n  \r\n ", vec![
            ("", "a", "\n"),
            ("  ", "b ", "\n"),
            ("", "c c", "\r\n"),
            ("  ", "", "\r\n"),
            (" ", "", "")
        ]);
    }
    #[test] fn line_re_contig() {
        line_re("\n\n\r\u{85}\n", vec![
            ("", "", "\n"),
            ("", "", "\n"),
            ("", "", "\r"),
            ("", "", "\u{85}"),
            ("", "", "\n")
        ]);
    }

    fn scan_test(input: &str, expected: Scanned) {
        let result = scan_string(input, Limits { max_indents: 1000 }).unwrap();

        let result_string: String = result.delimited_text.chars().map(|c| {
            match c {
                INDENT => '{',
                OUTDENT => '}',
                c => c
            }
        }).collect();
        
        assert_eq!(dbg!(result_string), expected.delimited_text);
        assert_eq!(&result.lines, &expected.lines);
    }
    macro_rules! scan {
        ($name:ident : $input:literal => $outstr:literal [$( [$ss:literal $rls:literal $ris:literal $rds:literal $rde:literal] )+]) => {
            #[test] fn $name() {
                scan_test($input, Scanned {
                    delimited_text: $outstr.into(),
                    lines: vec![
                        $(LineInfo {
                            scan_start: $ss,
                            raw_line_start: $rls,
                            raw_indent_start: $ris,
                            raw_data_start: $rds,
                            raw_data_end: $rde
                        }),+
                    ]
                });
            }
        }
    }

    scan!(no_lf_before_eof: "foo" => "foo\n" [
        [0 0 0 0 3]
    ]);

    scan!(basic_lines: "foo\nbar\nbaz\n" => "foo\nbar\nbaz\n" [
        [0 0 0 0 3]
        [4 4 4 4 7]
        [8 8 8 8 11]
    ]);

    scan!(basic_indents: "a\n  b\n   c\nb" => "a\n{b\n{c\n}}b\n" [
        [0 0 0 0 1]
        [2 2 2 4 5]
        [7 6 8 9 10]
        [12 11 11 11 12]
    ]);
}