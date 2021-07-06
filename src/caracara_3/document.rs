//! Abstract trees for Caracara documents

use std::marker::PhantomData;

mod traity {
    trait Document {
        type Node: Node;
        type Element: Element;
        type Text: AsRef<str>;

        type Children: Iterator<Item=Self::Node>;
        type ChildElements: Iterator<Item=Self::Element>;
        type TraverseBody: Iterator<Item=Self::Node>;

    }

    trait Node {
        fn as_type<T: Node>(&self) -> T;
    }

    trait Element: Node {
        //type ChildIterator: Iterator<Item=Node>;
        type ChildElementIterator: Iterator<Item=Self>;

        fn name(&self) -> &str;
        fn has_attribute(&self, name: &str) -> bool;
        fn attribute(&self, name: &str) -> &str;
        //fn head_children(&self) -> Self::ChildIterator;
        fn head_elements(&self) -> Self::ChildElementIterator;
        //fn body_children(&self) -> Self::ChildIterator;
        fn body_elements(&self) -> Self::ChildElementIterator;
    }
}

pub struct Document {}
impl Document {
    /// If implemented, recreate the document with surplus memory freed.
    pub fn gc(self) -> Self { self }


}

pub struct Element<'doc> { _doc: PhantomData<&'doc ()> }
impl<'doc> Element<'doc> {
    pub fn name(&self) -> &'doc str { todo!() }
    
    pub fn parent(&self) -> Element<'doc> { todo!() }
    pub fn next_sibling(&self) -> Element<'doc> { todo!() }
    pub fn prev_sibling(&self) -> Element<'doc> { todo!() }

    
}