use std::rc::Rc;

pub struct File {
    id: u32,
    name: Rc<str>,
    data: Rc<str>
}

/// Source-preserving &/Rc str.
pub enum TextChunk {
    /// String known at compile time
    Static { source: Span, data: &'static str },

    /// String can be retrieved by slicing into the source data.
    SourceSlice { source: Span, data: Rc<str> },

    /// String is on the heap.
    Heap { source: Span, data: Rc<str> }
}

/// Items that can have a span generated from them.
pub trait Spannable {
    fn span(&self) -> Span;
}


impl Spannable for TextChunk {
    fn span(&self) -> Span {
        todo!();
    }
}

/// Region of a file. Exact line/column information and bindings to the actual file buffer must be done externally, this
/// just stores enough information to recover that.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Span {
    file_id: u32,
    start: u32,
    end: u32
}
impl Span {
    /// Fuse abutting or overlapping spans
    pub fn try_merge(&self, other: &Span) -> Option<Span> {
        // different files
        if self.file_id != other.file_id { return None; }

        // not touching
        if self.start < other.start && self.end < other.start { return None; }
        if other.start < self.start && other.end < self.start { return None; }

        Some(self.superset(other))
    }

    pub fn try_merge_opt(l: Option<Span>, r: Option<Span>) -> Option<Span> {
        match l {
            None => r,
            Some(l) => match r {
                None => Some(l),
                Some(r) => l.try_merge(&r)
            }
        }
    }

    /// Make a span whose start and end encompass the other one as well.
    pub fn superset(&self, other: &Span) -> Span {
        Span {
            file_id: self.file_id,
            start: u32::min(self.start, other.start),
            end: u32::max(self.end, other.end)
        }
    }
}

