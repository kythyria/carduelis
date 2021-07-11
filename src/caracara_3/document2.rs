//! AST of Caracara documents
//!
//! There is no way to discern any purely syntactic distinctions from this, specifically to avoid any accidental dependencies on such.
//!
//! To simplify implementation, this is immutable outside of builders: No DOM editing, just rewrites of subtrees.

use std::{collections::HashMap, num::NonZeroUsize};

use super::spanned_string::SpannedString;

/// Caracara document
///
/// This actually holds the data.
pub struct Document {
    data: Vec<InternalNode>
}

pub enum NodeType {
    Root,
    Element,
    Text
}

/// Node of unspecified type
///
/// As a result, you can't go down, because only Elements have any
/// sort of children. But you can go up.
pub struct Node<'doc> {
    doc: &'doc Document,
    node_id: usize
}

/// An element's attributes.
pub struct Attributes<'doc>(Node<'doc>);

/// Iterator over successive parents
pub struct Ancestors<'doc>;

/// Iterator over next siblings
pub struct NextSiblings<'doc>;

/// Iterator over previous siblings
pub struct PrevSiblings<'doc>;

/// Iterator over descendants, descending via body
pub struct BodyDescendants<'doc>;

struct InternalNode {
    parent: ParentId,
    next_sibling: Option<NonZeroUsize>,
    prev_sibling: Option<NonZeroUsize>,
    ntype: InternalNodeType
}

enum InternalNodeType {
    Root,
    Text(SpannedString),
    Element {
        name: SpannedString,
        attributes: HashMap<SpannedString, SpannedString>,
        first_head: Option<NonZeroUsize>,
        last_head: Option<NonZeroUsize>,
        first_body: Option<NonZeroUsize>,
        last_body: Option<NonZeroUsize>
    }
}

enum ParentId {
    IsRoot,
    InHead(NonZeroUsize),
    InBody(usize)
}