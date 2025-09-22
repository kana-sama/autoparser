pub mod scanner;
pub mod syntax;
pub mod token;

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::scanner::scan;
    use super::syntax;
    use super::token::Token;

    #[test]
    fn test_scan() {
        let source = r#"
            begin
                foo;
                let _ : Bar = #some-tag( #true, bar, );
            end
        "#;

        let tokens = scan(source).unwrap();

        assert_matches!(tokens.as_slice(), [
            Token::Begin(_),
            Token::Ident(_),
            Token::Semicolon(_),
            Token::Let(_),
            Token::Underscore(_),
            Token::Colon(_),
            Token::Ident(_),
            Token::Equal(_),
            Token::Tag(_),
            Token::LParen(_),
            Token::Tag(_),
            Token::Comma(_),
            Token::Ident(_),
            Token::Comma(_),
            Token::RParen(_),
            Token::Semicolon(_),
            Token::End(_),
            Token::EOF(_),
        ]);
    }

    #[test]
    fn test_parse() {
        use crate::parser::parse;

        let source = r#"
            begin
                foo;
                let _ : Bar = #some-tag( #true, bar, );
            end
        "#;

        let tokens = scan(source).unwrap();
        let expr: syntax::Expr = parse(&tokens).unwrap();
        dbg!(expr);
    }

    #[test]
    fn test_parse_error() {
        use crate::parser::parse;

        let source = r#"
            begin
                foo;
                let q  Bar = #some-tag( #true, bar, );
            end
        "#;

        let tokens = scan(source).unwrap();
        let error = parse::<_, syntax::Expr>(&tokens).unwrap_err();

        println!("{}", error.pretty(source));
    }
}
