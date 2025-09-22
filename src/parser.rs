use itertools::Itertools as _;

use crate::separated::*;
use crate::token::*;

pub fn parse<Tok: TokenSet, P: Parse<Tok>>(tokens: &[Tok::Token]) -> Result<P, ParseError<Tok>> {
    let mut parser = Parser::new(tokens);
    parser.parse::<P>()
}

pub struct Parser<'tok, Tok: TokenSet> {
    pub tokens: &'tok [Tok::Token],
    pub cursor: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError<Tok: TokenSet> {
    UnexpectedToken {
        expected: Vec<Tok::Kind>,
        got: Option<Tok::Token>,
    },
}

impl<'tok, Tok: TokenSet> Parser<'tok, Tok> {
    pub fn new(tokens: &'tok [Tok::Token]) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub fn parse<P: Parse<Tok>>(&mut self) -> Result<P, ParseError<Tok>> {
        let result = P::parse(self)?;

        if self.cursor < self.tokens.len() {
            return Err(ParseError::UnexpectedToken {
                expected: vec![],
                got: Some(self.tokens[self.cursor].clone()),
            });
        }

        return Ok(result);
    }

    pub fn peek_some(&self) -> Option<&'tok Tok::Token> {
        if self.cursor >= self.tokens.len() {
            return None;
        }

        Some(&self.tokens[self.cursor])
    }

    pub fn assert<K: KindOfSet<Tok>>(&self) -> Result<&'tok TokenOfKind<K>, ParseError<Tok>> {
        if self.cursor >= self.tokens.len() {
            return Err(ParseError::UnexpectedToken {
                expected: vec![K::KIND],
                got: None,
            });
        }

        let current = &self.tokens[self.cursor];
        if let Some(t) = K::from_token(current) {
            return Ok(t);
        }

        return Err(ParseError::UnexpectedToken {
            expected: vec![K::KIND],
            got: Some(current.clone()),
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
            return Err(ParseError::UnexpectedToken {
                expected: kinds.to_vec(),
                got: None,
            });
        }

        let current = &self.tokens[self.cursor];

        if kinds.iter().contains(&Tok::kind(current)) {
            return Ok(current);
        }

        return Err(ParseError::UnexpectedToken {
            expected: kinds.to_vec(),
            got: Some(current.clone()),
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
    pub tokens: Vec<Tok::Kind>,
    pub can_be_empty: bool,
}

impl<Tok: TokenSet> First<Tok> {
    pub fn new(tokens: Vec<Tok::Kind>) -> Self {
        Self {
            tokens,
            can_be_empty: false,
        }
    }

    pub fn follow(mut self, other: impl FnOnce() -> Self) -> Self {
        if self.can_be_empty {
            let other = other();
            self.tokens.extend(other.tokens.clone());
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
    fn parse(parser: &mut Parser<Tok>) -> Result<Self, ParseError<Tok>>;
}

impl<Tok: TokenSet, K: KindOfSet<Tok>> Parse<Tok> for TokenOfKind<K>
where
    TokenOfKind<K>: Clone,
{
    fn first() -> First<Tok> {
        First::new(vec![K::KIND])
    }

    fn parse(parser: &mut Parser<Tok>) -> Result<Self, ParseError<Tok>> {
        let token = parser.take::<K>()?;
        return Ok(token.clone());
    }
}

impl<Tok: TokenSet, P: Parse<Tok>> Parse<Tok> for Option<P> {
    fn first() -> First<Tok> {
        P::first().optional()
    }

    fn parse(parser: &mut Parser<Tok>) -> Result<Self, ParseError<Tok>> {
        let node = if parser.peek_one_of(&Self::first().tokens).is_some() {
            Some(P::parse(parser)?)
        } else {
            None
        };

        return Ok(node);
    }
}

impl<Tok: TokenSet, P: Parse<Tok>> Parse<Tok> for Vec<P> {
    fn first() -> First<Tok> {
        P::first().optional()
    }

    fn parse(parser: &mut Parser<Tok>) -> Result<Self, ParseError<Tok>> {
        let mut nodes = Vec::new();

        while parser.peek_one_of(&Self::first().tokens).is_some() {
            nodes.push(P::parse(parser)?);
        }

        return Ok(nodes);
    }
}

impl<Tok: TokenSet, P1: Parse<Tok>, P2: Parse<Tok>> Parse<Tok> for (P1, P2) {
    fn first() -> First<Tok> {
        P1::first().follow(P2::first)
    }

    fn parse(parser: &mut Parser<Tok>) -> Result<Self, ParseError<Tok>> {
        return Ok((P1::parse(parser)?, P2::parse(parser)?));
    }
}

impl<Tok: TokenSet, P: Parse<Tok>> Parse<Tok> for Box<P> {
    fn first() -> First<Tok> {
        P::first()
    }

    fn parse(parser: &mut Parser<Tok>) -> Result<Self, ParseError<Tok>> {
        return Ok(Box::new(P::parse(parser)?));
    }
}

impl<Tok: TokenSet, P: Parse<Tok>, Sep: KindOfSet<Tok>> Parse<Tok> for NonEmptySeparated<P, Sep> {
    fn first() -> First<Tok> {
        P::first()
    }

    fn parse(parser: &mut Parser<Tok>) -> Result<Self, ParseError<Tok>> {
        let first = P::parse(parser)?;
        let rest = Option::parse(parser)?;
        return Ok(NonEmptySeparated { first, rest });
    }
}

impl<Tok: TokenSet, P: Parse<Tok>, Sep: KindOfSet<Tok>> Parse<Tok> for NonEmptySeparatedRest<P, Sep>
where
    TokenOfKind<Sep>: Clone,
{
    fn first() -> First<Tok> {
        First::new(vec![Sep::KIND])
    }

    fn parse(parser: &mut Parser<Tok>) -> Result<Self, ParseError<Tok>> {
        let sep = parser.take::<Sep>()?.clone();

        if parser.peek_one_of(&P::first().tokens).is_some() {
            let t = P::parse(parser)?;

            if parser.peek::<Sep>().is_some() {
                let rest = Parse::parse(parser)?;

                return Ok(NonEmptySeparatedRest {
                    sep,
                    rest: Some((t, Some(Box::new(rest)))),
                });
            }
        } else {
            return Ok(NonEmptySeparatedRest { sep, rest: None });
        }

        return Err(ParseError::UnexpectedToken {
            expected: P::first().tokens,
            got: parser.peek_some().map(Clone::clone),
        });
    }
}

impl<Tok: TokenSet, P: Parse<Tok>, Sep: KindOfSet<Tok>> Parse<Tok> for Separated<P, Sep> {
    fn first() -> First<Tok> {
        P::first().optional()
    }

    fn parse(parser: &mut Parser<Tok>) -> Result<Self, ParseError<Tok>> {
        if parser.peek_one_of(&Self::first().tokens).is_some() {
            return Ok(Separated::NonEmpty(Parse::parse(parser)?));
        }

        return Ok(Separated::Empty);
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

            fn parse(parser: &mut $crate::parser::Parser<$Tok>) -> Result<Self, $crate::parser::ParseError<$Tok>> {
                derive_parse! { field parser [ $i1 : $t1, $( $in : $tn, )* ] }

                return Ok(Self {
                    $i1,
                    $( $in, )*
                });
            }
        }
    };

    (field $parser:ident [$i1:ident : $t1:ty, $i2:ident : $t2:ty, $( $in:ident : $tn:ty, )*]) => {
        let $i1 = $crate::parser::Parse::parse($parser)?;
        derive_parse!(field $parser [ $i2 : $t2, $( $in : $tn, )* ])
    };

    (field $parser:ident [$i1:ident : $t1:ty,]) => {
        let $i1 = $crate::parser::Parse::parse($parser)?;
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
                let mut tokens = Vec::new();
                $(
                    tokens.extend(<$ty>::first().tokens);
                )*

                $crate::parser::First::new(tokens)
            }

            fn parse(parser: &mut $crate::parser::Parser<$Tok>) -> Result<Self, $crate::parser::ParseError<$Tok>> {
                $(
                    if parser.peek_one_of(&<$ty>::first().tokens).is_some() {
                        return Ok($name::$arm($crate::parser::Parse::parse(parser)?));
                    }
                )*

                return Err($crate::parser::ParseError::UnexpectedToken {
                    expected: Self::first().tokens.clone(),
                    got: parser.peek_some().map(Clone::clone),
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
    fn test_expr() {
        use crate::syntax::token::t;
        use crate::syntax::*;

        #[rustfmt::skip]
        let tokens = vec![
            t!["begin"],
                t!["ident"], t![";"],
                t!["let"], t!["_"], t![":"], t!["ident"], t!["="],
                    t!["#tag"], t!["("],
                        t!["#tag"], t![","],
                        t!["ident"], t![","],
                    t![")"],
                t![";"],
            t!["end"],
            t!["EOF"],
        ];

        let expr: Expr = parse(&tokens).unwrap();

        dbg!(expr);
    }

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
                    "a?" => A,
                    "b"  => B,
                    "c?" => C,
                    "d"  => D,
                    ";"  => Semicolon,
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

        assert_matches!(parse(&[t!["a?"], t!["b"]]).unwrap(), S1::B((Some(_), _)));
        assert_matches!(parse(&[t!["b"]]).unwrap(), S1::B((None, _)));
        assert_matches!(parse(&[t!["c?"], t!["d"]]).unwrap(), S1::D((Some(_), _)));
        assert_matches!(parse(&[t!["d"]]).unwrap(), S1::D((None, _)));

        assert!(parse::<_, S1>(&[t!["a?"]]).is_err());
        assert!(parse::<_, S1>(&[t!["c?"]]).is_err());
        assert!(parse::<_, S1>(&[t!["a?"], t!["d"]]).is_err());
        assert!(parse::<_, S1>(&[t!["c?"], t!["b"]]).is_err());

        #[derive(Debug)]
        #[apply(derive_parse)]
        #[token_set(tok::Set)]
        pub struct S2 {
            pub b: Vec<T!["b"]>,
            pub d: Vec<T!["d"]>,
            pub e: Option<T![";"]>,
        }

        assert_matches!(parse(&[t!["b"], t!["b"]]).unwrap(), S2 { b, d, e } if b.len() == 2 && d.len() == 0 && e.is_none());
        assert_matches!(parse(&[t!["b"], t!["d"]]).unwrap(), S2 { b, d, e } if b.len() == 1 && d.len() == 1 && e.is_none());
        assert_matches!(parse(&[t!["d"], t!["d"]]).unwrap(), S2 { b, d, e } if b.len() == 0 && d.len() == 2 && e.is_none());
        assert_matches!(parse(&[t!["b"], t!["b"], t![";"]]).unwrap(), S2 { b, d, e } if b.len() == 2 && d.len() == 0 && e.is_some());
        assert_matches!(parse(&[t!["b"], t!["d"], t![";"]]).unwrap(), S2 { b, d, e } if b.len() == 1 && d.len() == 1 && e.is_some());
        assert_matches!(parse(&[t!["d"], t!["d"], t![";"]]).unwrap(), S2 { b, d, e } if b.len() == 0 && d.len() == 2 && e.is_some());
    }
}

// const fn concat<T: Copy, const LEN: usize>(a: &[T], b: &[T]) -> [T; LEN] {
//     let mut result = [std::mem::MaybeUninit::uninit(); LEN];
//     let mut i;

//     i = 0;
//     while i < a.len() {
//         result[i] = std::mem::MaybeUninit::new(a[i]);
//         i += 1;
//     }

//     i = 0;
//     while i < b.len() {
//         result[a.len() + i] = std::mem::MaybeUninit::new(b[i]);
//         i += 1;
//     }

//     return unsafe { std::mem::MaybeUninit::array_assume_init(result) };
// }
