use std::marker::PhantomData;

use rowan::{GreenNode, GreenNodeBuilder, SyntaxKind};

pub trait FixedValues {
    fn try_fixed_value(&self) -> Option<&'static str>;
}

pub trait GreenBuildExt<SK> {
    fn interior<R>(&mut self, kind: SK, body: impl FnOnce(&mut Self) -> R) -> R;
    fn leaf(&mut self, kind: SK, val: &str);
    fn kw(&mut self, kind: SK);
}
impl<'cache, SK> GreenBuildExt<SK> for GreenNodeBuilder<'cache>
where
    SK: Into<SyntaxKind> + FixedValues + std::fmt::Debug
{
    fn interior<R>(&mut self, kind: SK, body: impl FnOnce(&mut Self) -> R) -> R {
        self.start_node(kind.into());
        let res = body(self);
        self.finish_node();
        res
    }

    fn leaf(&mut self, kind: SK, val: &str) {
        self.token(kind.into(), val);
    }

    fn kw(&mut self, kind: SK) {
        match &kind.try_fixed_value() {
            Some(v) => self.leaf(kind, v),
            None => panic!("Not a keyword-like: {:?}", kind)
        }
    }
}

pub fn cst<'a, SK, B>(kind: SK, body: B) -> GreenNode
where
    SK: Into<SyntaxKind>,
    B: FnOnce(&mut GreenNodeBuilder<'a>)
{
    let mut b = GreenNodeBuilder::new();
    b.start_node(kind.into());
    body(&mut b);
    b.finish_node();
    b.finish()
}

pub fn format_cst<'a, L>(node: &'a GreenNode) -> impl std::fmt::Display + 'a
where
    L: rowan::Language + 'a,
    L::Kind: std::fmt::Debug
{
    PrintCstTree::<L>(node, PhantomData{})
}

pub struct PrintCstTree<'a, L>(pub &'a GreenNode, std::marker::PhantomData<L>);

impl<'a, L> std::fmt::Display for PrintCstTree<'a, L>
where
    L: rowan::Language,
    L::Kind: std::fmt::Debug
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let indent_step = f.precision().unwrap_or(2);
        let mut indent = 0;

        let sn = rowan::cursor::SyntaxNode::new_root(self.0.clone());
        
        for evt in sn.preorder_with_tokens() {
            use rowan::{WalkEvent::*, NodeOrToken::*};
            match evt {
                Enter(Node(n)) => {
                    let fnam = if indent == 0 { "cst" } else { "b.interior" };
                    let kind = L::kind_from_raw(n.kind());
                    writeln!(f, "{0:1$}{2}({3:?}, |b|{{", "", indent, fnam, kind)?;
                    indent += indent_step;
                },
                Leave(Node(_)) => {
                    indent -= indent_step;
                    writeln!(f, "{0:1$}}});", "", indent)?;
                },
                Enter(Token(t)) => {
                    let kind = L::kind_from_raw(t.kind());
                    writeln!(f, "{0:1$}b.leaf({2:?},{3:?});", "", indent, kind, t.text())?;
                },
                Leave(Token(_)) => {}
            }
        }
        Ok(())
    }
}