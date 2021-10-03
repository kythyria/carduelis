//! Abstract trees for Caracara documents

use std::num::NonZeroUsize;
use std::collections::BTreeMap;

#[derive(Clone, Copy)]
struct Span(u32, u32, u32);

#[derive(Clone)]
pub struct SpannedString {
    data: String,
    spans: Vec<Span>
}
impl SpannedString {
    fn from_static(s: &str) -> Self {
        SpannedString {
            data: String::from(s),
            spans: Vec::new()
        }
    }
}

/// A document
///
/// Internally this works a lot like `ego-tree`: Everything is in a big Vec and
/// uses indices to refer to each other. External callers are given a reference
/// to the document itself and the index, with the API pretending that it's the
/// real thing.
///
/// Index zero is the root node, although you never see this. It's just so that
/// zero is never the sibling or child, which makes those smaller.
pub struct Document {
    data: Vec<Node>
}
impl Document {
    /// If implemented, recreate the document with surplus memory freed.
    pub fn gc(self) -> Self { self }

    /// Get the data in the document
    pub fn roots(&self) -> FragmentIter {
        if self.data.len() == 0 {
            return FragmentIter::new_empty(self);
        }
        match &self.data[0].data {
            NodeData::EmptyRoot => FragmentIter::new_empty(self),
            NodeData::Root(rd) => FragmentIter::new(self, rd.first_child, rd.last_child),
            _ => panic!("Document doesn't start with a root!")
        }
    }
}

struct Node {
    parent: usize,
    next_sibling: Option<NonZeroUsize>,
    prev_sibling: Option<NonZeroUsize>,
    data: NodeData
}

enum NodeData {
    EmptyRoot,
    Root(RootData),
    Text(String),
    Element {
        name: String,
        head: Option<(NonZeroUsize, NonZeroUsize)>,
        body: Option<(NonZeroUsize, NonZeroUsize)>,
        attributes: BTreeMap<String, String>
    }
}

struct RootData {
    first_child: NonZeroUsize,
    last_child: NonZeroUsize,
}

struct ElementData {
    name: String,
    head: Option<(NonZeroUsize, NonZeroUsize)>,
    body: Option<(NonZeroUsize, NonZeroUsize)>,
    attributes: BTreeMap<String, String>
}

macro_rules! node_ref_struct {
    ($n:ident) => {
        pub struct $n<'doc> {
            doc: &'doc Document,
            node_id: usize
        }

        impl<'doc> $n<'doc> {
            pub fn document(&self) -> &'doc Document { self.doc }
    
            pub fn parent(&self) -> Option<NodeRef<'doc>> {
                match self.doc.data[self.node_id].parent {
                    0 => None,
                    p => Some(NodeRef { doc: self.doc, node_id: p })
                }
            }
            pub fn next_sibling(&self) -> Option<NodeRef<'doc>> {
                match self.doc.data[self.node_id].next_sibling {
                    None => None,
                    Some(p) => Some(NodeRef { doc: self.doc, node_id: p.get() })
                }
            }
            pub fn prev_sibling(&self) -> Option<NodeRef<'doc>> {
                match self.doc.data[self.node_id].prev_sibling {
                    None => None,
                    Some(p) => Some(NodeRef { doc: self.doc, node_id: p.get() })
                }
            }
        }
    }
}

node_ref_struct!(NodeRef);
impl<'doc> NodeRef<'doc> {
    pub fn details(&self) -> NodeRefType<'doc> {
        match self.doc.data[self.node_id].data {
            NodeData::EmptyRoot => panic!("How did you get a NodeRef to an EmptyRoot?"),
            NodeData::Root{..} => panic!("How did you get a NodeRef to a Root?"),
            NodeData::Element{..} => NodeRefType::Element(ElementRef {
                doc: self.doc, node_id: self.node_id
            }),
            NodeData::Text{..} => NodeRefType::Text(TextRef {
                doc: self.doc, node_id: self.node_id
            })
        }
    }
}


macro_rules! with_nodedata {
    ($s:ident, $nt:ident $v:tt => $code:expr) => {
        match &$s.doc.data[$s.node_id].data {
            NodeData::$nt $v => $code,
            _ => panic!(concat!("Node type changed out from under us (should be ", stringify!($nt), ")"))
        }
    }
}

pub enum NodeRefType<'doc> {
    Element(ElementRef<'doc>),
    Text(TextRef<'doc>)
}

node_ref_struct!(ElementRef);
impl<'doc> ElementRef<'doc> {
    pub fn name(&self) -> &str {
        with_nodedata!(self, Element{name, ..} => &name)
    }
    pub fn head(&self) -> FragmentIter<'doc> {
        with_nodedata!(self, Element{ head, ..} => match head {
            Some(head) => FragmentIter::new(self.doc, head.0, head.1),
            None => FragmentIter::new_empty(self.doc)
        })
    }
    pub fn body(&self) -> FragmentIter<'doc> {
        match &self.doc.data[self.node_id].data {
            NodeData::Element{ body, .. } => match body {
                Some(body) => FragmentIter::new(self.doc, body.0, body.1),
                None => FragmentIter::new_empty(self.doc)
            },
            _ => panic!("Node type changed out from under us (should be Element)")
        }
    }
}

node_ref_struct!(TextRef);
impl<'doc> TextRef<'doc> {
    pub fn text(&self) -> &'doc str {
        match &self.doc.data[self.node_id].data {
            NodeData::Text(s) => s,
            _ => panic!("Node type changed out from under us (should be Text")
        }
    }
}

pub struct FragmentIter<'doc> {
    doc: &'doc Document,
    next_id: Option<NonZeroUsize>,
    final_id: Option<NonZeroUsize>
}
impl<'doc> FragmentIter<'doc> {
    fn new_empty(doc: &'doc Document) -> Self {
        FragmentIter {
            doc,
            next_id: None, final_id: None
        }
    }

    fn new(doc: &'doc Document, next_id: NonZeroUsize, final_id: NonZeroUsize) -> Self {
        FragmentIter {
            doc,
            next_id: Some(next_id),
            final_id: Some(final_id)
        }
    }
}

impl<'doc> Iterator for FragmentIter<'doc> {
    type Item = NodeRef<'doc>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_id {
            None => None,
            Some(nid) => {
                if self.next_id != self.final_id {
                    self.next_id = self.doc.data[nid.get()].next_sibling;
                }
                else {
                    self.next_id = None;
                    self.final_id = None;
                }
                Some(NodeRef { doc: self.doc, node_id: nid.get()})
            }
        }
    }
}
impl<'doc> DoubleEndedIterator for FragmentIter<'doc> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.final_id {
            None => None,
            Some(nid) => {
                if self.final_id != self.next_id {
                    self.final_id = self.doc.data[nid.get()].prev_sibling;
                }
                else {
                    self.final_id = None;
                    self.next_id = None;
                }
                Some(NodeRef { doc: self.doc, node_id: nid.get()})
            }
        }
    }
}