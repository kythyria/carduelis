/// An object that can produce a document or fragment
///
/// While this does not impose an order, it's probably a good idea to do the
/// head-attributes-body order that the concrete syntax does.
pub trait TreeBuilder {
    type Success;
    type Error;
    type TextArg;
    type Document;

    fn begin_element(&mut self, name: Self::TextArg) -> Result<Self::Success, Self::Error>;
    fn attribute(&mut self, name: Self::TextArg, value: Self::TextArg) -> Result<Self::Success, Self::Error>;
    fn begin_head(&mut self) -> Result<Self::Success, Self::Error>;
    fn begin_body(&mut self) -> Result<Self::Success, Self::Error>;
    fn end_element(&mut self) -> Result<Self::Success, Self::Error>;

    fn text(&mut self, value: Self::TextArg) -> Result<Self::Success, Self::Error>;

    fn finish(self) -> Result<Self::Document, Self::Error>;
}

pub mod simple_rc {
    //! The most simple document.
    //!
    //! It's about the second naivest way to store all this: `Vec<Rc>` all over the
    //! place, `HashMap` for attributes, that sort of thing. Text is just a string,
    //! no source tracking.

    use std::cell::{Cell, RefCell};
    use std::collections::HashMap;
    use std::rc::{Rc, Weak};

    pub struct Document;

    struct Element {
        parent: Cell<Weak<Node>>,
        next: Cell<Weak<Node>>,
        prev: Cell<Weak<Node>>,

        name: String,
        attributes: HashMap<String, String>,
        head: Vec<Rc<Node>>,
        body: Vec<Rc<Node>>,
    }

    struct Text {
        parent: Cell<Weak<Node>>,
        next: Cell<Weak<Node>>,
        prev: Cell<Weak<Node>>,

        text: String
    }

    struct Root {
        children: Vec<Rc<Node>>
    }

    enum NodeType {
        Root(Root),
        Element(Element),
        Text(Text)
    }

    type Node = RefCell<NodeType>;
}

pub mod ego_unspanned {
    //! Document implemented with `ego_tree`. Here the head and body are reified as
    //! their own nodes for ease of implementation. Text is plain strings.

    use ego_tree::*;

    pub struct Document(Tree<NodeType>);

    pub enum NodeType {
        Root,
        Element,
        Head,
        Body,
        Text
    }
}