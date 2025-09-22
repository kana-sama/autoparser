use itertools::Itertools as _;

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
                let q Bar = #some-tag( #true, bar, );
            end
        "#;

        let tokens = scan(source).unwrap();
        let error = parse::<_, syntax::Expr>(&tokens).unwrap_err();

        println!("{}", super::pretty_parse_error(source, error));
    }
}

fn pretty_parse_error(source: &str, error: crate::parser::ParseError<token::Set>) -> String {
    use annotate_snippets::*;

    let report = match &error {
        crate::parser::ParseError::UnexpectedToken { expected, got } => {
            let found = match got {
                Some(got) => format!("`{}`", got.to_source_string(source)),
                None => "end of input".to_string(),
            };

            let loc = got.as_ref().map(|token| token.loc());

            let span = match loc {
                Some(loc) => loc.range(),
                None => (source.len()..source.len()).into(),
            };

            let title = format!(
                "expected {}, found {}",
                expected
                    .into_iter()
                    .map(|k| format!("{:?}", k.to_string()))
                    .join(" or "),
                found,
            );

            let annotation = AnnotationKind::Primary.span(span).highlight_source(true);

            Level::ERROR
                .primary_title(title)
                .element(Snippet::source(source).annotation(annotation))
        }
    };

    let renderer = Renderer::styled().decor_style(renderer::DecorStyle::Unicode);

    renderer.render(&[report])
}
