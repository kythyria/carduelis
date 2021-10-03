struct PeekLexer<'s, T: logos::Logos<'s>>(Option<(T, &'s str)>, logos::Lexer<'s, T>);
impl<'s, T: logos::Logos<'s>> PeekLexer<'s, T> {
    fn new(input: &'s str) -> Self {
        Self(None, logos::Lexer::<T>::new(input))
    }
}

impl<'s, T: logos::Logos<'s>> Iterator for PeekLexer<'s, T> {
    type Item = (T, &'s str);
    
}