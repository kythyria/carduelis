use std::convert::TryInto;

use lazy_static::lazy_static;
use regex::{Regex};

use super::ParseError;
use super::annotated_string::AnnotatedString;

pub const NEWLINE: char = '\n';
pub const INDENT: char = '\u{FDD0}';
pub const OUTDENT: char = '\u{FDD1}';
pub const MISDENT: char = '\u{FDD2}';
pub const EOF: char = '\u{FFFF}';
pub const INDENT_LIMIT: usize = 1000;

pub struct AnalysedString {
    pub lines: Vec<LineInfo>,
    pub data: String,
    pub spans: Vec<CharInfo>
}

pub struct LineInfo {
    pub source_start: u32,
    pub source_data: u32,
    pub source_end: u32,
    pub analysed_start: u32,
    pub analysed_data: u32,
    pub analysed_end: u32,
    pub indent_level: u32
}

impl LineInfo {
    pub fn has_data(&self) -> bool {
        self.analysed_data < self.analysed_end
    }
}

pub struct CharInfo {
    pub source_start: u32,
    pub source_end: u32,
    pub line: u32,
}

#[derive(Clone, Copy)]
pub struct AnalysedSlice<'a> {
    pub string: &'a AnalysedString,
    pub start: u32,
    pub end: u32
}

impl<'a> nom::InputTake for AnalysedSlice<'a> {
    fn take(&self, count: usize) -> Self {
        let count: u32 = count.try_into().unwrap();

        if self.start + count > self.end {
            panic!("Tried to take({}) from a {} byte AnalysedSlice", count, self.end - self.start);
        }

        AnalysedSlice {
            string: self.string,
            start: self.start,
            end: self.start + count
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let first = self.take(count);
        let rest = AnalysedSlice {
            string: self.string,
            start: first.end,
            end: self.end
        };

        (rest, first)
    }
}

lazy_static! {
    static ref LINE_RE: Regex = Regex::new(r"(?P<indent>[\p{White_Space}&&[^\r\n\u{000C}\u{000B}\u{2028}\u{2029}\u{0085}]]*)(?P<data>.*?)(?P<eol>\r\n|[\r\n\u{000C}\u{000B}\u{2028}\u{2029}\u{0085}]|$)").unwrap();
}

pub fn scan_string(input: &str) -> Result<AnnotatedString, ParseError> {
    // As a consequence of this check, all offsets will fit inside u32, as will the line count:
    // there can't be less than one character per line.
    if input.len() >= u32::MAX as usize { return Err(ParseError::InputTooLong) }

    let mut res = AnnotatedString::with_capacity(input.len());

    let mut indents = vec![""];

    for (curr_line, line_cap) in LINE_RE.captures_iter(input).enumerate() {

        let indent_m = line_cap.name("indent").unwrap();
        let data_m = line_cap.name("data").unwrap();
        let eol_m = line_cap.name("eol").unwrap();

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
                        res.push(INDENT, indent_m.end() - remaining_indent.len(), indent_m.end());
                        if indents.len() > INDENT_LIMIT { return Err(ParseError::TooManyIndents(curr_line as u32)) }
                        break;
                    }
                    else if remaining_indent.len() == 0 && i < indents.len()-1 {
                        // outdent
                        let to_pop = (indents.len() - 1) - i;
                        for _ in 0..to_pop {
                            indents.pop();
                            res.push(OUTDENT, indent_m.start(), indent_m.end());
                        }
                        break;
                    }
                    else {
                        // don't know yet
                        continue;
                    }
                }
                else {
                    // misdent
                    let to_pop = (indents.len() - 1) - i;
                    for _ in 0..to_pop {
                        indents.pop();
                        res.push(OUTDENT, indent_m.start(), indent_m.end() - remaining_indent.len());
                    }
                    res.push(MISDENT, indent_m.end() - remaining_indent.len(), indent_m.end());
                    break;
                }
            }
        }

        res.push_str_filtered(data_m.as_str(), data_m.start(), |c: char| {
            let ci = c as u32 & 0xFFFF;
            if ci == 0xFFFF { '\u{FFFD}' }
            else if ci == 0xFFFE { '\u{FFFD}' }
            else if ('\u{FDD0}' ..= '\u{FDEF}').contains(&c) { '\u{FFFD}' }
            else { c }
        });
        res.push(NEWLINE, eol_m.start(), eol_m.end());
        res.push_line(indent_m.start(), data_m.start(), eol_m.end());
    }

    while indents.len() > 1 {
        res.push(OUTDENT, input.len(), input.len());
    }

    res.push(EOF, input.len(), input.len());
    
    Ok(res)
}

#[cfg(test)]
mod tests {
    fn line_re(testitem: &str, expect: Vec<(&str, &str, &str)>) {
        let test_items: Vec<_> = super::LINE_RE.captures_iter(testitem).map(|m| (
            m.name("indent").unwrap().as_str(),
            m.name("data").unwrap().as_str(),
            m.name("eol").unwrap().as_str()
        )).collect();

        assert_eq!(test_items, expect);
    }

    #[test]
    fn line_re_spacing() {
        line_re("a\n  b \nc c\r\n  \r\n ", vec![
            ("", "a", "\n"),
            ("  ", "b ", "\n"),
            ("", "c c", "\r\n"),
            ("  ", "", "\r\n"),
            (" ", "", "")
        ]);
    }

    #[test]
    fn line_re_contig() {
        line_re("\n\n\r\u{85}\n", vec![
            ("", "", "\n"),
            ("", "", "\n"),
            ("", "", "\r"),
            ("", "", "\u{85}"),
            ("", "", "\n")
        ]);
    }
}