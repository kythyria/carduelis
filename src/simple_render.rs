//! Simple transformation to HTML
//! 
//! This is a super hardcoded prototype.
//! Transformation is as follows:
//! 
//! # Paragraph formation.
//! 
//! Elements are classified as block, inline, or transparent, on the outside.
//! 
//! On the inside, they are paragraphing, non-paragraphing, or lazy.
//! 
//! Non-paragraphing elements do nothing special. Paragraphing elements execute
//! the grouping algorithm:
//! * A run of text, inline elements, transparent elements, and single newlines
//!   is a paragraph block.
//! * Two or more consecutive newlines force a block boundary.
//! * Block elements, and transparent elements that are the only thing in their
//!   block, are blocks.
//! * Paragraph blocks get reified as `\p` elements.
//! * Lazy elements don't do that last step if that would result in them having
//!   exactly one child.
//! 
//! # Header
//! Documents can start with the `\doc` element. Its head is the page title.
//! 
//! The body can contain:
//! 
//! | Element              | Use                                |
//! |----------------------|------------------------------------|
//! | `\author{a}`         | Document author (text only).       |
//! | `\description{d}`    | Document description (text only)   |
//! | `\link(rel){href}`   | `<link>` element (attrs like HTML) |
//! | `\meta(name){val}`   | `<meta>` element                   |
//! | `\script(src){code}` | Inline JS                          |
//! | `\style{code}`       | Inline CSS                         |
//! 
//! # Document content
//! 
//! | Name      | Paragraphing | Block? | Function |
//! |-----------|------|-------------|-------------------------------------------------|
//! |`\a`       | N    | Inline      | Link, head is target URL. |
//! |`\code`    | N    | Block       | Block of source code. Head is language, emit as `data-lang`. |
//! |`\c`       | N    | Inline      | Inline monospace text: `<code>` |
//! |`\details` | Lazy | Block       | `<details>`, head (lazy) is the `<summary>` |
//! |`\dl`      | N    | Block       | Definition list. |
//! |`\em`      | N    | Inline      | Emphasis. |
//! |`\fn`      | N    | Inline      | Footnote reference, head is ID of footnote. |
//! |`\footnote`| Lazy | Block       | Define a footnote, head is ID, body is contents. |
//! |`\html`    | N    | Transparent | Raw HTML. Heads are never used. |
//! |`\img`     | N    | Transparent | `<img>`, head is `src`, body is `alt` |
//! |`\li`      | Lazy | Block       | List item. In `\dl`, head is `<dt>` and body is `<dd>`. |
//! |`\ol`      | N    | Block       | Ordered list. `start` attribute to override start number. |
//! |`\pre`     | N    | Block       | Block of text to render monospace. |
//! |`\p`       | N    | Block       | Explicit paragraph. |
//! |`\q`       | N    | Inline      | Inline quote, `<q>` |
//! |`\rem`     | N    | Transparent | Comment. Never output. |
//! |`\section` | Y    | Block       | `<section>`, head (lazy) is the section header. |
//! |`\strong`  | N    | Inline      | Strong emphasis. |
//! |`\quote`   | Lazy | Transparent | Quotation. If head (lazy) given, gives source. |
//! |`\ul`      | N    | Block       | Unordered list. |
//! 
//! `\pre` and `\code` trim their input of leading and trailing line breaks and
//! Newlines, as well as common indentation prefix on the left.
//! 
//! # ToC and footnotes
//! Table of contents is printed before the first `\section`, in a `<nav>`.
//! 
//! Footnotes are appended at the end in a big `<aside>`.

use crate::caracara_6 as cc;

enum OuterType {
    /// Must be inside a paragraph or paragraph-like
    Inline,

    /// Must be outside a paragraphoid.
    Block,

    /// Can be inside or outside a paragraphoid: if it would be the only thing
    /// in the paragraph, no paragraph is generated.
    Either
}
enum InnerType {
    /// Never paragraphise contents
    Inline,

    /// Always paragraphise contents
    Block,

    /// Paragraphise contents if there is a double newline in there
    Lazy,

    /// Should contain nothing. Nothing, I tell you!
    Empty
}
enum HeadUse {
    None,
    Element(&'static str),
    Attribute(&'static str)
}
enum ProcessType {
    Simple(&'static str, InnerType, HeadUse),
    Custom(fn(cc::Element, &mut RenderContext) -> Vec<cc::Node>)
}
enum SectioningType {
    Body,
    Heading,
    Section,
    SecRoot
}
struct ElementInfo {
    name: &'static str,
    outside_type: OuterType,
    processing: ProcessType,
    sectioning: SectioningType
}

macro_rules! element_table {
    ($($name:literal: $outside:ident, $st:ident, $it:ident( $($ib:tt)* );)* ) => {
        const ELEMENTS: &[ElementInfo] = &[ $(
            ElementInfo {
                name: $name,
                outside_type: OuterType::$outside,
                processing: element_table!(@process $it $($ib)*),
                sectioning: SectioningType::$st
            }
        ),*];
    };
    (@process Simple $elem:literal, $intype:ident, $hu:ident$(($hup:literal))?) => {
        ProcessType::Simple($elem, InnerType::$intype, HeadUse::$hu$(($hup))?)
    };
    (@process Custom $fun:expr) => {
        ProcessType::Custom($fun)
    };
    ($($i:tt)*) => {};
}

// Several elements are in this table mainly for completion, you wouldn't actually
// type them most of the time.

element_table! {
    "+":          Inline, Body,    Simple("ins",        Inline, None);
    "-":          Inline, Body,    Simple("del",        Inline, None);
    "a":          Inline, Body,    Simple("a",          Inline, Attribute("href"));
    "abbr":       Inline, Body,    Simple("abbr",       Inline, Attribute("title"));
    "area":       Either, Body,    Simple("area",       None,   Attribute("href"));
    "address"     Either, Body,    Simple("address",    Lazy,   None);
    "article":    Block,  Section, Custom(render_section);
    "aside":      Block,  Section, Custom(render_section);
    "audio":      Block,  Body,    Custom(render_media);
    "b":          Inline, Body,    Simple("b",          Inline, None);
    "bdi":        Inline, Body,    Simple("bdi",        Inline, None);
    "bdo":        Inline, Body,    Simple("bdo",        Inline, None);
    "blockquote": Block,  SecRoot, Simple("blockquote", Lazy,   Attribute("cite"));
    "br":         Inline, Body,    Simple("br",         Empty,  None);
    "button":     Either, Body,    Simple("button",     Inline, None);
    "canvas":     Either, Body,    Simple("canvas",     Lazy,   None);
    "caption":    Block,  Body,    Simple("caption",    Block,  None);
    "cite":       Inline, Body,    Simple("cite",       Inline, None);
    "code":       Either, Body,    Simple("code",       Inline, None);
    "col":        Block,  Body,    Simple("col",        Empty,  None);
    "colgroup":   Block,  Inline,  Simple("colgroup",   Inline, None);
    "data":       Block,  Body,    Simple("data",       Inline, Attribute("value"));
    "datalist":   Either, Body,    Simple("datalist",   Inline, Attribute("id"));
    "dd":         Block,  Body,    Simple("dd",         Lazy,   None);
    "del":        Inline, Body,    Simple("del",        Inline, None);
    "details":    Block,  SecRoot, Simple("details",    Lazy,   Element("summary"));
    "dfn":        Inline, Body,    Simple("dfn",        Inline, Attribute("title"));
    "dialog":     Block,  Body,    Simple("dialog",     Block,  None);
    "div":        Block,  Body,    Simple("div",        Block,  None);
    "dl":         Block,  Body,    Custom(render_definition_list);
    "dt":         Block,  Body,    Simple("dt",         Lazy,   None);
    "em":         Inline, Body,    Simple("em",         Inline, None);
    "embed":      Either, Body,    Simple("embed"       Lazy,   Attribute("src"));
    "fieldset":   Block,  SecRoot, Simple("fieldset",   Block,  Element("legend"));
    "figcaption": Block,  Body,    Simple("figcaption", Lazy,   None);
    "figure":     Block,  SecRoot, Custom(render_figure);
    "fn":         Inline, Body,    Custom(render_fnref);
    "footer":     Block,  Body,    Simple("footer",     Block,  None);
    "footnote":   Block,  Body,    Custom(accumulate_footnote);
    "form":       Block,  Body,    Simple("form",       Block,  None);
    "h1":         Block,  Heading, Simple("h1",         Block,  None);
    "h2":         Block,  Heading, Simple("h2",         Block,  None);
    "h3":         Block,  Heading, Simple("h3",         Block,  None);
    "h4":         Block,  Heading, Simple("h4",         Block,  None);
    "h5":         Block,  Heading, Simple("h5",         Block,  None);
    "h6":         Block,  Heading, Simple("h6",         Block,  None);
    "header":     Block,  Body,    Simple("header",     Block,  None);
    "hgroup":     Block,  Heading, Simple("hgroup",     Block,  None);
    "hr":         Block,  Body,    Simple("hr",         None,   None);
    "i":          Inline, Body,    Simple("i",          Inline, None);
    "iframe":     Block,  Body,    Simple("iframe",     None,   Attribute("src"));
    "img":        Either, Body,    Custom(render_image);
    "input":      Inline, Body,    Simple("input",      None,   None);
    "ins":        Inline, Body,    Simple("ins",        Inline, None);
    "kbd":        Either, Body,    Simple("kbd",        Inline, None);
    "label":      Inline, Body,    Simple("label",      Inline, Attribute("for"));
    "legend"      Block,  Body,    Simple("legend",     Inline, None)
    "li":         Block,  Body,    Simple("li",         Lazy,   None);
    "link":       Either, Body,    Custom(render_link);
    "main":       Block,  Body,    Simple("body",       Block,  None);
    "map":        Either, Body,    Simple("map",        Lazy,   None);
    "mark":       Inline, Body,    Simple("mark",       Inline, None);
    "math":       Either, Body,    Custom(render_math);
    "menu":       Block,  Body,    Simple("menu",       Inline, None);
    "meta":       Either, Body,    Custom(render_meta);
    "meter"       Either, Body,    Simple("meter",      Inline, None);
    "nav":        Block,  Section, Simple("nav",        Block,  None);
    "noscript":   Either, Body,    Simple("noscript",   Lazy,   None);
    "object":     Either, Body,    Simple("object",     Lazy,   None);
    "ol":         Block,  Body,    Simple("ol",         Inline, None);
    "optgroup"    Either, Body,    Simple("optgroup",   Inline, Attribute("label"));
    "section":    Block,  Section, Custom(render_section);
    "showtoc":    Block,  Body,    Custom(render_toc);
    "shownotes":  Block,  Body,    Custom(render_footnotes);
    "table":      Block,  Body,    Custom(render_table);
    "time":       Inline, Body,    Simple("time",    Inline, Attribute("datetime"));
    "video":      Block,  Body,    Custom(render_media);
}

struct RenderContext;



macro_rules! element_list {
    ($( $(#[$meta:meta])* $name:ident = $($item:ident)+ );+ ) => {
        $(
            $(#[$meta])*
            const $name: &[&str] = &[ $(stringify!($item)),+ ];
        )+
    };
}

element_list!(
    /// Elements that occur inside a paragraph when paragraph conversion is happening, and thus
    /// get wrapped in a paragraph.
    PHRASING_ELEMENTS =
    a abbr audio b bdi bdo br button canvas cite code data datalist del dfn em
    embed i iframe img input ins kbd label link map mark math meta meter
    noscript object output picture progress q ruby s samp script select slot 
    small span strong sub sup svg template textarea time u var video wbr
);

/// Convert n-1 parsed newlines to n paragraph breaks, and paragraph breaks into paragraphs.
fn paragraphise(mut input: Vec<cc::Node>) -> Vec<cc::Node> {
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Item {
        Phrasing,
        Flow,
        PotentialBreak
    }

    fn classify(el: &cc::Element) -> Item {
        if PHRASING_ELEMENTS.contains(&el.name.name.as_str()) {
            Item::Phrasing
        }
        else {
            Item::Flow
        }
    }

    let breaked = GroupWithKeysMut::new(input.as_mut_slice(), |item| match item {
        cc::Node::Newline(_) => Item::PotentialBreak,
        cc::Node::Element(e) => classify(e),
        cc::Node::Text(_) => Item::Phrasing,
    }).filter(|(key,items)| items.len() != 0 && !(Item::PotentialBreak == *key && items.len() == 1));

    let mut output = Vec::new();
    // this way, we don't have to worry about the starting case
    // since none of the flow combinations ever need to look at the previous item
    let last_type = Item::Flow;

    for (curr_type, items) in breaked {
        match (last_type, curr_type) {
            (Item::Flow, Item::Phrasing) => output.push(items_into_paragraph(items)),
            (Item::Phrasing, Item::Phrasing) => append_to_element(output.last_mut().unwrap(), items),
            (Item::PotentialBreak, Item::Phrasing) => append_to_element(output.last_mut().unwrap(), items),
            (_, Item::Flow) => drain_items_into(items, &mut output),
            (_, Item::PotentialBreak) => start_paragraphs(&mut output, items),
        }
    }

    output
}

fn start_paragraphs(target: &mut Vec<cc::Node>, items: &mut [cc::Node]) {
    for i in items.iter_mut().skip(1) {
        target.push(cc::Node::Element(cc::Element {
            name: cc::Name{ name: String::from("p"), span: i.span().implied_by_start() },
            attributes: std::collections::HashMap::new(),
            head: Vec::new(),
            body: Vec::new()
        }))
    }
}

fn append_to_element(existing: &mut cc::Node, items: &mut [cc::Node]) {
    let elem = match existing {
        cc::Node::Element(e) => e,
        _ => panic!("Expected an element"),
    };
    drain_items_into(items, &mut elem.body);
}

fn items_into_paragraph(items: &mut [cc::Node]) -> cc::Node {
    let start_span = items[0].span();
    cc::Node::Element(cc::Element {
        name: cc::Name { name: String::from("p"), span: start_span.implied_by_start() },
        attributes: std::collections::HashMap::new(),
        head: Vec::new(),
        body: drain_items(items),
    })
}

fn drain_items(items: &mut [cc::Node]) -> Vec<cc::Node> {
    let mut res = Vec::new();
    drain_items_into(items, &mut res);
    res
}

fn drain_items_into(items: &mut [cc::Node], target: &mut Vec<cc::Node>) {
    target.reserve(items.len());
    for i in items.iter_mut() {
        let item = std::mem::replace(i, cc::Node::Newline(cc::Newline { span: cc::Span::default() }));
        target.push(item);
    }
}

struct GroupWithKeysMut<'a, I, P, K>
where
    P: Fn(&I) -> K,
    K: Clone + PartialEq
{
    remaining: &'a mut [I],
    grouper: P,
}
impl<'a, I, P, K> GroupWithKeysMut<'a, I,P,K>
where
    P: Fn(&I) -> K,
    K: Clone + PartialEq
{
    fn new(slice: &'a mut [I], grouper: P) -> Self {
        GroupWithKeysMut {
            remaining: slice,
            grouper
        }
    }
}

impl<'a, I, P, K> Iterator for GroupWithKeysMut<'a, I,P,K>
where
    P: Fn(&I) -> K,
    K: Clone + PartialEq
{
    type Item = (K, &'a mut [I]);
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        let mut iter = self.remaining.iter();

        let mut key = iter.next().map(&self.grouper)?;
        let mut len = 1;
        while let Some(n) = iter.next() {
            let kn = (self.grouper)(n);
            if key == kn {
                len += 1;
                key = kn;
            }
            else {
                break;
            }
        }
        let slice = std::mem::take(&mut self.remaining);
        let (head, tail) = slice.split_at_mut(len);
        self.remaining = tail;
        Some((key, head))
    }
}