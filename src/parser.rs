use itertools::Itertools as _;

use crate::separated::*;
use crate::token::*;

pub fn parse<Tok: TokenSet, P: Parse<Tok>>(tokens: &[Tok::Token]) -> Result<P, ParseError<Tok>> {
    let mut parser = Parser::new(tokens);
    P::parse(&mut parser, Follow::new(vec![Tok::EOF]))
}

pub struct Parser<'tok, Tok: TokenSet> {
    pub tokens: &'tok [Tok::Token],
    pub cursor: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError<Tok: TokenSet> {
    UnexpectedToken {
        expected: Vec<Tok::Kind>,
        found:    Option<Tok::Token>,
    },
}

impl<Tok: TokenSet> ParseError<Tok> {
    pub fn pretty(self, source: &str) -> String {
        use annotate_snippets::*;

        let report = match self {
            crate::parser::ParseError::UnexpectedToken { expected, found } => {
                let expected = if expected.len() == 1 {
                    format!("{:?}", expected[0].to_string())
                } else {
                    let heads = expected
                        .iter()
                        .dropping_back(1)
                        .map(|k| format!("{:?}", k.to_string()))
                        .join(", ");
                    let tail = format!("{:?}", expected.last().unwrap().to_string());
                    format!("{} or {}", heads, tail)
                };

                let span = found
                    .as_ref()
                    .map(|token| token.loc())
                    .unwrap_or(source.len()..source.len());

                let found = match found {
                    None => "end of input".to_string(),
                    Some(found) if found.loc().is_empty() => "end of input".to_string(),
                    Some(found) => format!("`{}`", found.to_source_string(source)),
                };

                let title = format!("expected {expected}, found {found}");

                let annotation = AnnotationKind::Primary
                    .span(span.clone())
                    .highlight_source(true);

                let prev_line_start = source[..span.start].rfind('\n').map_or(0, |i| i - 1);

                Level::ERROR.primary_title(title).element(
                    Snippet::source(source)
                        .annotation(AnnotationKind::Visible.span(prev_line_start..span.start))
                        .annotation(annotation),
                )
            }
        };

        Renderer::styled().render(&[report])
    }
}

impl<'tok, Tok: TokenSet> Parser<'tok, Tok> {
    pub fn new(tokens: &'tok [Tok::Token]) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub fn peek_some(&self) -> Option<&'tok Tok::Token> {
        if self.cursor >= self.tokens.len() {
            return None;
        }

        Some(&self.tokens[self.cursor])
    }

    pub fn assert<K: KindOfSet<Tok>>(&self) -> Result<&'tok TokenOfKind<K>, ParseError<Tok>> {
        if self.cursor >= self.tokens.len() {
            return Err(ParseError::UnexpectedToken { expected: vec![K::KIND], found: None });
        }

        let current = &self.tokens[self.cursor];
        if let Some(t) = K::from_token(current) {
            return Ok(t);
        }

        return Err(ParseError::UnexpectedToken {
            expected: vec![K::KIND],
            found:    Some(current.clone()),
        });
    }

    pub fn peek<K: KindOfSet<Tok>>(&self) -> Option<&'tok TokenOfKind<K>> {
        self.assert::<K>().ok()
    }

    pub fn take<K: KindOfSet<Tok>>(&mut self) -> Result<&'tok TokenOfKind<K>, ParseError<Tok>> {
        let token = self.assert::<K>();

        if token.is_ok() {
            self.cursor += 1;
        }

        return token;
    }

    pub fn assert_one_of(&self, kinds: &[Tok::Kind]) -> Result<&'tok Tok::Token, ParseError<Tok>> {
        if self.cursor >= self.tokens.len() {
            return Err(ParseError::UnexpectedToken { expected: kinds.to_vec(), found: None });
        }

        let current = &self.tokens[self.cursor];

        if kinds.iter().contains(&Tok::kind(current)) {
            return Ok(current);
        }

        return Err(ParseError::UnexpectedToken {
            expected: kinds.to_vec(),
            found:    Some(current.clone()),
        });
    }

    pub fn peek_one_of(&self, kinds: &[Tok::Kind]) -> Option<&'tok Tok::Token> {
        self.assert_one_of(kinds).ok()
    }

    pub fn try_take<K: KindOfSet<Tok>>(&mut self) -> Option<&'tok TokenOfKind<K>> {
        let token = self.peek::<K>();

        if token.is_some() {
            self.cursor += 1;
        }

        return token;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct First<Tok: TokenSet> {
    pub tokens:       Vec<Tok::Kind>,
    pub can_be_empty: bool,
}

pub type Follow<Tok> = First<Tok>;

impl<Tok: TokenSet> First<Tok> {
    pub fn new(tokens: Vec<Tok::Kind>) -> Self {
        Self { tokens, can_be_empty: false }
    }

    pub fn follow(mut self, other: impl FnOnce() -> Self) -> Self {
        if self.can_be_empty {
            let other = other();
            self.tokens.extend(other.tokens);
            self.can_be_empty = other.can_be_empty;
        }

        return self;
    }

    pub fn optional(mut self) -> Self {
        self.can_be_empty = true;
        return self;
    }
}

pub trait Parse<Tok: TokenSet>: Sized {
    fn first() -> First<Tok>;
    fn parse(parser: &mut Parser<Tok>, follow: Follow<Tok>) -> Result<Self, ParseError<Tok>>;
}

impl<Tok: TokenSet, K: KindOfSet<Tok>> Parse<Tok> for TokenOfKind<K> {
    fn first() -> First<Tok> {
        First::new(vec![K::KIND])
    }

    fn parse(parser: &mut Parser<Tok>, follow: Follow<Tok>) -> Result<Self, ParseError<Tok>> {
        let token = parser.take::<K>()?;

        if parser.peek_one_of(follow.tokens.as_slice()).is_some() {
            return Ok(token.clone());
        }

        return Err(ParseError::UnexpectedToken {
            expected: [follow.tokens].concat(),
            found:    parser.peek_some().cloned(),
        });
    }
}

impl<Tok: TokenSet, P: Parse<Tok>> Parse<Tok> for Option<P> {
    fn first() -> First<Tok> {
        P::first().optional()
    }

    fn parse(parser: &mut Parser<Tok>, follow: Follow<Tok>) -> Result<Self, ParseError<Tok>> {
        if parser.peek_one_of(&Self::first().tokens).is_some() {
            return Ok(Some(P::parse(parser, follow)?));
        }

        if parser.peek_one_of(&follow.tokens).is_some() {
            return Ok(None);
        }

        return Err(ParseError::UnexpectedToken {
            expected: [Self::first().tokens, follow.tokens].concat(),
            found:    parser.peek_some().cloned(),
        });
    }
}

impl<Tok: TokenSet, P: Parse<Tok>> Parse<Tok> for Vec<P>
where
    Tok::Kind: std::fmt::Debug,
{
    fn first() -> First<Tok> {
        P::first().optional()
    }

    fn parse(parser: &mut Parser<Tok>, follow: Follow<Tok>) -> Result<Self, ParseError<Tok>> {
        let mut nodes = Vec::new();

        loop {
            if parser.peek_one_of(&Self::first().tokens).is_some() {
                nodes.push(P::parse(parser, Self::first().follow(|| follow.clone()))?);
                continue;
            }

            if parser.peek_one_of(&follow.tokens).is_some() {
                return Ok(nodes);
            }

            break;
        }

        return Err(ParseError::UnexpectedToken {
            expected: [Self::first().tokens, follow.tokens].concat(),
            found:    parser.peek_some().cloned(),
        });
    }
}

impl<Tok: TokenSet, P1: Parse<Tok>, P2: Parse<Tok>> Parse<Tok> for (P1, P2) {
    fn first() -> First<Tok> {
        P1::first().follow(P2::first)
    }

    fn parse(parser: &mut Parser<Tok>, follow: Follow<Tok>) -> Result<Self, ParseError<Tok>> {
        return Ok((
            P1::parse(parser, P2::first().follow(|| follow.clone()))?,
            P2::parse(parser, follow)?,
        ));
    }
}

impl<Tok: TokenSet, P: Parse<Tok>> Parse<Tok> for Box<P> {
    fn first() -> First<Tok> {
        P::first()
    }

    fn parse(parser: &mut Parser<Tok>, follow: Follow<Tok>) -> Result<Self, ParseError<Tok>> {
        return Ok(Box::new(P::parse(parser, follow)?));
    }
}

impl<Tok: TokenSet, P: Parse<Tok>, Sep: KindOfSet<Tok>> Parse<Tok> for Separated<P, Sep> {
    fn first() -> First<Tok> {
        P::first().optional()
    }

    fn parse(parser: &mut Parser<Tok>, follow: Follow<Tok>) -> Result<Self, ParseError<Tok>> {
        if parser.peek_one_of(&Self::first().tokens).is_some() {
            return Ok(Separated::NonEmpty(Parse::parse(parser, follow)?));
        }

        if parser.peek_one_of(&follow.tokens).is_some() {
            return Ok(Separated::Empty);
        }

        return Err(ParseError::UnexpectedToken {
            expected: [Self::first().tokens, follow.tokens].concat(),
            found:    parser.peek_some().cloned(),
        });
    }
}

impl<Tok: TokenSet, P: Parse<Tok>, Sep: KindOfSet<Tok>> Parse<Tok> for NonEmptySeparated<P, Sep> {
    fn first() -> First<Tok> {
        P::first()
    }

    fn parse(parser: &mut Parser<Tok>, follow: Follow<Tok>) -> Result<Self, ParseError<Tok>> {
        let trailing_comma_follow = Follow::new(vec![Sep::KIND])
            .optional()
            .follow(|| follow.clone());

        let first = P::parse(parser, trailing_comma_follow.clone())?;

        let mut trailing = None;
        let mut rest = Vec::new();

        loop {
            let sep = Parse::parse(parser, P::first().optional().follow(|| follow.clone()))?;

            let Some(sep) = sep else {
                break;
            };

            if parser.peek_one_of(&P::first().tokens).is_none() {
                trailing = Some(sep);
                break;
            }

            let item = P::parse(parser, trailing_comma_follow.clone())?;

            rest.push((sep, item));
        }

        if parser.peek_one_of(&follow.tokens).is_some() {
            return Ok(NonEmptySeparated { first, rest, trailing });
        }

        return Err(ParseError::UnexpectedToken {
            expected: [Self::first().tokens, follow.tokens].concat(),
            found:    parser.peek_some().cloned(),
        });
    }
}

#[allow(unused)]
macro_rules! derive_parse {
    (
        #[token_set($Tok:ty)]
        $( #[$_1:meta] )*
        $vis:vis
        struct $name:ident {
            $( #[$_2:meta] )*
            pub $i1:ident : $t1:ty,
            $(
                $( #[$_4:meta] )*
                pub $in:ident : $tn:ty,
            )*
        }
    ) => {
        $( #[$_1] )*
        $vis
        struct $name {
            $( #[$_2] )*
            pub $i1 : $t1,
            $(
                $( #[$_4] )*
                pub $in : $tn,
            )*
        }

        impl $crate::parser::Parse<$Tok> for $name {
            fn first() -> $crate::parser::First<$Tok> {
                <$t1>::first() $( .follow( <$tn>::first ) )*
            }

            fn parse(parser: &mut $crate::parser::Parser<$Tok>, follow: $crate::parser::Follow<$Tok>) -> Result<Self, $crate::parser::ParseError<$Tok>> {
                derive_parse! { field parser follow [ $i1 : $t1, $( $in : $tn, )* ] }
                return Ok(Self { $i1, $( $in, )* });
            }
        }
    };

    (field $parser:ident $follow:ident [$i1:ident : $t1:ty, $i2:ident : $t2:ty, $( $in:ident : $tn:ty, )*]) => {
        let $i1 = $crate::parser::Parse::parse($parser, <$t2>::first() $( .follow(|| <$tn>::first()) )* .follow(|| $follow.clone()) )?;
        derive_parse!(field $parser $follow [ $i2 : $t2, $( $in : $tn, )* ])
    };

    (field $parser:ident $follow:ident [$i1:ident : $t1:ty,]) => {
        let $i1 = $crate::parser::Parse::parse($parser, $follow)?;
    };

    (
        #[token_set($Tok:ty)]
        $( #[$meta:meta] )*
        $vis:vis
        enum $name:ident {
            $(
                $( #[$arm_meta:meta] )*
                $arm:ident($ty:ty),
            )*
        }
    ) => {
        $( #[$meta] )*
        $vis
        enum $name {
            $(
                $( #[$arm_meta] )*
                $arm($ty),
            )*
        }

        impl $crate::parser::Parse<$Tok> for $name {
            fn first() -> $crate::parser::First<$Tok> {
                $crate::parser::First::new([ $( <$ty>::first().tokens ),* ].concat())
            }

            fn parse(parser: &mut $crate::parser::Parser<$Tok>, follow: $crate::parser::Follow<$Tok>) -> Result<Self, $crate::parser::ParseError<$Tok>> {
                $(
                    if parser.peek_one_of(&<$ty>::first().tokens).is_some() {
                        return Ok($name::$arm($crate::parser::Parse::parse(parser, follow)?));
                    }
                )*

                return Err($crate::parser::ParseError::UnexpectedToken {
                    expected: Self::first().tokens.clone(),
                    found: parser.peek_some().map(Clone::clone),
                });
            }
        }
    };
}

#[allow(unused)]
pub(crate) use derive_parse;

#[cfg(test)]
mod tests {
    use super::*;

    use macro_rules_attribute::apply;
    use std::assert_matches::assert_matches;

    #[test]
    pub fn test_follow_is_not_necessary() {
        // https://stackoverflow.com/a/70118055
        // тут утверждается что FOLLOW нужен для LL(1) в общем случае
        // у меня есть интуиция что это не так, но нужно проверить

        // мое предположение что FOLLOW нужен только для того чтобы упасть с ошибкой
        // в нужном месте, а не где-то после

        token_set! {
            pub mod tok {
                pub struct Set;

                pub mod kinds {
                    "a?"  => A,
                    "b"   => B,
                    "c?"  => C,
                    "d"   => D,
                    ";"   => Semicolon,
                }
            }
        }

        use tok::{T, t};

        #[derive(Debug)]
        #[apply(derive_parse)]
        #[token_set(tok::Set)]
        pub enum S1 {
            B((Option<T!["a?"]>, T!["b"])),
            D((Option<T!["c?"]>, T!["d"])),
        }

        #[rustfmt::skip] {
            assert_matches!(parse(&[t!["a?"], t!["b"], t![EOF]]).unwrap(),S1::B((Some(_), _)));
            assert_matches!(parse(&[t!["b"], t![EOF]]).unwrap(), S1::B((None, _)));
            assert_matches!(parse(&[t!["c?"], t!["d"], t![EOF]]).unwrap(), S1::D((Some(_), _)));
            assert_matches!(parse(&[t!["d"], t![EOF]]).unwrap(), S1::D((None, _)));

            assert!(parse::<_, S1>(&[t!["a?"], t![EOF]]).is_err());
            assert!(parse::<_, S1>(&[t!["c?"], t![EOF]]).is_err());
            assert!(parse::<_, S1>(&[t!["a?"], t!["d"], t![EOF]]).is_err());
            assert!(parse::<_, S1>(&[t!["c?"], t!["b"], t![EOF]]).is_err());
        }

        #[derive(Debug)]
        #[apply(derive_parse)]
        #[token_set(tok::Set)]
        pub struct S2 {
            pub b: Vec<T!["b"]>,
            pub d: Vec<T!["d"]>,
            pub e: Option<T![";"]>,
        }

        assert_matches!(parse(&[t!["b"], t!["b"], t![EOF]]).unwrap(), S2 { b, d, e: None } if b.len() == 2 && d.len() == 0);
        assert_matches!(parse(&[t!["b"], t!["d"], t![EOF]]).unwrap(), S2 { b, d, e: None } if b.len() == 1 && d.len() == 1);
        assert_matches!(parse(&[t!["d"], t!["d"], t![EOF]]).unwrap(), S2 { b, d, e: None } if b.len() == 0 && d.len() == 2);
        assert_matches!(parse(&[t!["b"], t!["b"], t![";"], t![EOF]]).unwrap(), S2 { b, d, e: Some(_) } if b.len() == 2 && d.len() == 0);
        assert_matches!(parse(&[t!["b"], t!["d"], t![";"], t![EOF]]).unwrap(), S2 { b, d, e: Some(_) } if b.len() == 1 && d.len() == 1);
        assert_matches!(parse(&[t!["d"], t!["d"], t![";"], t![EOF]]).unwrap(), S2 { b, d, e: Some(_) } if b.len() == 0 && d.len() == 2);
    }
}
