use std::ops::Range;
use std::convert::TryInto;

use nom::{Compare, InputIter, InputLength, InputTake, InputTakeAtPosition};

/// String annotated with source span and line information.
///
/// It relies rather heavily on the source spans never going backwards
#[derive(Default)]
pub struct AnnotatedString {
    chars: String,
    lines: Vec<LineInfo>,
    char_info: Vec<CharInfo>
}

impl AnnotatedString {
    pub fn with_capacity(charcount: usize) -> AnnotatedString {
        AnnotatedString {
            chars: String::with_capacity(charcount),
            lines: Vec::new(),
            char_info: Vec::with_capacity(charcount)
        }
    }

    pub fn push<T>(&mut self, c: char, start: T, end: T)
    where
        T: TryInto<u32>,
        <T as TryInto<u32>>::Error: std::fmt::Debug
    {
        self.chars.push(c);
        for _ in 0..(c.len_utf8()) {
            self.char_info.push(CharInfo {
                source_start: start.try_into().unwrap(),
                source_end: end.try_into().unwrap()
            })
        }
    }

    pub fn push_str<T>(&mut self, s: &str, start: T)
    where
        T: TryInto<u32>,
        <T as TryInto<u32>>::Error: std::fmt::Debug
    {
        self.chars.push_str(s);
        self.lines.reserve(s.len());
        for i in 0..s.len() {
            let si: u32 = i.try_into().unwrap();
            self.char_info.push(CharInfo {
                source_start: start.try_into().unwrap() + si,
                source_end: start.try_into().unwrap() + si + 1
            })
        }
    }

    pub fn push_str_filtered<T,F>(&mut self, s: &str, start: S, filter: F)
    where
        T: TryInto<u32>,
        F: FnMut(char) -> char
    {
        self.chars.reserve(s.len());
        self.char_info.reserve(s.len());

        for (i, c) in s.char_indices() {
            let si: u32 = i.try_into().unwrap();
            self.chars.push(filter(c));
            self.char_info.push(CharInfo {
                source_start: start.try_into().unwrap() + si,
                source_end: start.try_into().unwrap() + si + 1
            })
        }
    }

    pub fn push_line<T: TryInto<u32>>(&mut self, start: T, data: T, end: T)
    where
        T: TryInto<u32>,
        <T as TryInto<u32>>::Error: std::fmt::Debug
    {
        let data_start = self.lines.last().map(|i| i.span.end).unwrap_or(0);
        let data_end: u32 = self.chars.len().try_into().unwrap();

        self.lines.push(LineInfo {
            span: data_start..data_end,
            source_line_start: start.try_into().unwrap(),
            source_data_start: data.try_into().unwrap(),
            source_line_end: end.try_into().unwrap()
        })
    }

    pub fn to_slice<'a>(&'a self) -> AnnotatedSlice<'a> {
        AnnotatedSlice {
            string: self,
            start: 0,
            end: self.chars.len().try_into().unwrap()
        }
    }

    pub fn annotate_slice<'a>(&'a self, slice: &'a str) -> AnnotatedSlice<'a> {
        let me_start = self.chars.as_ptr() as usize;
        let sl_start = slice.as_ptr() as usize;
        let start = sl_start - me_start;
        if sl_start < me_start || start + slice.len() > self.chars.len() {
            panic!("Slice is not from this string")
        }
        AnnotatedSlice {
            string: self,
            start: start as u32,
            end: (start + slice.len()) as u32
        }
    }
}

struct LineInfo {
    span: Range<u32>,
    source_line_start: u32,
    source_data_start: u32,
    source_line_end: u32
}

struct CharInfo {
    source_start: u32,
    source_end: u32
}

#[derive(Clone, Copy)]
pub struct AnnotatedSlice<'a> {
    string: &'a AnnotatedString,
    start: u32,
    end: u32
}
impl<'a> AnnotatedSlice<'a> {
    pub fn as_str(&self) -> &'a str {
        &self.string.chars.as_str()[(self.start as usize)..(self.end as usize)]
    }

    pub fn source_span(&self) -> (u32, u32) {
        let ci = &self.string.char_info;
        (ci[self.start as usize].source_start, ci[self.end as usize].source_end)
    }

    pub fn len(&self) -> u32 {
        self.end - self.start
    }

    pub fn start(&self) -> u32 {
        self.start
    } 
}

impl<'a> InputTake for AnnotatedSlice<'a> {
    fn take(&self, count: usize) -> Self {
        if count > self.len() as usize {
            panic!("take({}) doesn't fit span {}..{}", count, self.start, self.end);
        }

        AnnotatedSlice {
            string: self.string,
            start: self.start,
            end: self.start + (count as u32)
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let first = self.take(count);
        let rest = AnnotatedSlice {
            string: self.string,
            start: first.end,
            end: self.end
        };
        (rest, first)
    }
}

impl<'a> InputLength for AnnotatedSlice<'a> {
    fn input_len(&self) -> usize {
        (self.end - self.start) as usize
    }
}

impl<'a> InputIter for AnnotatedSlice<'a> {
    type Item = char;
    type Iter = std::str::CharIndices<'a>;
    type IterElem = std::str::Chars<'a>;

    fn iter_indices(&self) -> Self::Iter {
        self.as_str().char_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.as_str().chars()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool
    {
        self.as_str().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.as_str().slice_index(count)
    }
}

impl<'a> Compare<&str> for AnnotatedSlice<'a> {
    fn compare(&self, t: &str) -> nom::CompareResult {
        self.as_str().compare(t)
    }

    fn compare_no_case(&self, t: &str) -> nom::CompareResult {
        self.as_str().compare_no_case(t)
    }
}