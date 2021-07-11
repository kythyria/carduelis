pub struct SpannedString(Data);

enum Data {
    SynthInline {
        len: u8,
        data: [u8; 38]
    },

    UniformInline { 
        len: u8,
        data: [u8; 25],
        // explode the Span so that it can be better packed
        // Synth is handled by Data::SynthInline
        is_atom: bool,
        src_file: u32,
        src_start: u32,
        src_end: u32
    },

    UniformHeap {
        data: Box<str>,
        span: Span
    },

    VariedInline {
        len: u8,
        data: [u8; 22],
        spans: Box<[SpanData]>
    },

    VariedHeap {
        data: Box<str>,
        spans: Box<[SpanData]>
    }
}

struct SpanData {
    data_start: u32,
    span: Span
}

enum Span {
    /// Conjured ex nihilo by code.
    Synth,

    /// Data corresponds to source, but not in a sliceable way.
    Atom { src_file: u32, src_start: u32, src_end: u32 },

    /// Data is a straight copy of the source.
    Slice { src_file: u32, src_start: u32, src_end: u32 }
}
