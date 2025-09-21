use crate::token::*;

pub struct Parser<'tok> {
    pub tokens: &'tok [SomeToken],
    pub cursor: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    UnexpectedToken {
        expected: Vec<SomeTokenKind>,
        got: Option<SomeToken>,
    },
}

impl<'tok> Parser<'tok> {
    pub fn new(tokens: &'tok [SomeToken]) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub fn peek_some(&self) -> Option<&'tok SomeToken> {
        if self.cursor >= self.tokens.len() {
            return None;
        }

        Some(&self.tokens[self.cursor])
    }

    pub fn peek<T>(&self) -> Option<&'tok T>
    where
        T: TokenTrait,
        for<'a> &'a T: TryFrom<&'a SomeToken>,
    {
        self.assert::<T>().ok()
    }

    pub fn peek_one_of(&self, kinds: &[SomeTokenKind]) -> Option<&'tok SomeToken> {
        self.assert_one_of(kinds).ok()
    }

    pub fn assert<T>(&self) -> Result<&'tok T, ParseError>
    where
        T: TokenTrait,
        for<'a> &'a T: TryFrom<&'a SomeToken>,
    {
        if self.cursor >= self.tokens.len() {
            return Err(ParseError::UnexpectedToken {
                expected: vec![T::KIND],
                got: None,
            });
        }

        let current = &self.tokens[self.cursor];
        if let Ok(t) = <&T>::try_from(current) {
            return Ok(t);
        }

        return Err(ParseError::UnexpectedToken {
            expected: vec![T::KIND],
            got: Some(current.clone()),
        });
    }

    pub fn assert_one_of(&self, kinds: &[SomeTokenKind]) -> Result<&'tok SomeToken, ParseError> {
        if self.cursor >= self.tokens.len() {
            return Err(ParseError::UnexpectedToken {
                expected: kinds.to_vec(),
                got: None,
            });
        }

        let current = &self.tokens[self.cursor];
        for kind in kinds {
            if current.kind() == *kind {
                return Ok(current);
            }
        }

        return Err(ParseError::UnexpectedToken {
            expected: kinds.to_vec(),
            got: Some(current.clone()),
        });
    }

    pub fn take<T>(&mut self) -> Result<&'tok T, ParseError>
    where
        T: TokenTrait,
        for<'a> &'a T: TryFrom<&'a SomeToken>,
    {
        let token = self.assert::<T>();

        if token.is_ok() {
            self.cursor += 1;
        }

        return token;
    }

    pub fn try_take<T>(&mut self) -> Option<&'tok T>
    where
        T: TokenTrait,
        for<'a> &'a T: TryFrom<&'a SomeToken>,
    {
        let token = self.peek::<T>();

        if token.is_some() {
            self.cursor += 1;
        }

        return token;
    }
}

pub trait Parse: Sized {
    const FIRST: &[SomeTokenKind];
    fn parse(parser: &mut Parser, follow: &[SomeTokenKind]) -> Result<Self, ParseError>;
}

impl<K: Clone + 'static> Parse for TokenStruct<K>
where
    TokenStruct<K>: TokenTrait,
    for<'a> &'a TokenStruct<K>: TryFrom<&'a SomeToken>,
{
    const FIRST: &[SomeTokenKind] = &[TokenStruct::<K>::KIND];

    fn parse(parser: &mut Parser, _follow: &[SomeTokenKind]) -> Result<Self, ParseError> {
        let token = parser.take::<Self>()?;
        return Ok(token.clone());
    }
}

impl<T: Parse> Parse for Option<T> {
    const FIRST: &[SomeTokenKind] = T::FIRST;

    fn parse(parser: &mut Parser, follow: &[SomeTokenKind]) -> Result<Self, ParseError> {
        let node = if parser.peek_one_of(Self::FIRST).is_some() {
            Some(T::parse(parser, follow)?)
        } else {
            None
        };

        return Ok(node);
    }
}

impl<T: Parse> Parse for Vec<T> {
    const FIRST: &[SomeTokenKind] = T::FIRST;

    fn parse(parser: &mut Parser, follow: &[SomeTokenKind]) -> Result<Self, ParseError> {
        let mut nodes = Vec::new();

        while parser.peek_one_of(follow).is_none() {
            nodes.push(T::parse(parser, follow)?);
        }

        return Ok(nodes);
    }
}

impl<T: Parse, G: Parse> Parse for (T, G) {
    const FIRST: &[SomeTokenKind] = T::FIRST;

    fn parse(parser: &mut Parser, follow: &[SomeTokenKind]) -> Result<Self, ParseError> {
        return Ok((T::parse(parser, G::FIRST)?, G::parse(parser, follow)?));
    }
}

impl<T: Parse> Parse for Box<T> {
    const FIRST: &[SomeTokenKind] = T::FIRST;

    fn parse(parser: &mut Parser, follow: &[SomeTokenKind]) -> Result<Self, ParseError> {
        return Ok(Box::new(T::parse(parser, follow)?));
    }
}

macro_rules! ParseDerive {
    (
        $( #[$_1:meta] )*
        $vis:vis struct $name:ident {
            $( #[$_2:meta] )*
            pub $i1:ident : $t1:ty,
            $(
                $( #[$_4:meta] )*
                pub $in:ident : $tn:ty,
            )*
        }
    ) => {
        impl $crate::parser::Parse for $name {
            const FIRST: &[crate::token::SomeTokenKind] = <$t1>::FIRST;

            fn parse(parser: &mut $crate::parser::Parser, follow: &[crate::token::SomeTokenKind]) -> Result<Self, $crate::parser::ParseError> {
                ParseDerive! { field parser follow [ $i1 : $t1, $( $in : $tn, )* ] }

                return Ok(Self {
                    $i1,
                    $( $in, )*
                });
            }
        }
    };

    (field $parser:ident $follow:ident [$i1:ident : $t1:ty, $i2:ident : $t2:ty, $( $in:ident : $tn:ty, )*]) => {
        let $i1 = $crate::parser::Parse::parse($parser, <$t2>::FIRST)?;
        ParseDerive!(field $parser $follow [ $i2 : $t2, $( $in : $tn, )* ])
    };

    (field $parser:ident $follow:ident [$i1:ident : $t1:ty,]) => {
        let $i1 = $crate::parser::Parse::parse($parser, $follow)?;
    };

    (
        $( #[$_1:meta] )*
        $vis:vis enum $name:ident {
            $(
                $( #[$_2:meta] )*
                $arm:ident($ty:ty),
            )*
        }
    ) => {
        impl $crate::parser::Parse for $name {
            const FIRST: &[crate::token::SomeTokenKind] = constcat::concat_slices!([crate::token::SomeTokenKind]:
                $( <$ty>::FIRST, )*
            );

            fn parse(parser: &mut $crate::parser::Parser, follow: &[crate::token::SomeTokenKind]) -> Result<Self, $crate::parser::ParseError> {
                $(
                    if parser.peek_one_of(<$ty>::FIRST).is_some() {
                        return Ok($name::$arm($crate::parser::Parse::parse(parser, follow)?));
                    }
                )*

                return Err($crate::parser::ParseError::UnexpectedToken {
                    expected: Self::FIRST.to_vec(),
                    got: parser.peek_some().map(Clone::clone),
                });
            }
        }
    };
}

pub(crate) use ParseDerive;

#[test]
fn test() {
    use kinds::*;

    fn t<K>(_: K) -> SomeToken
    where
        SomeToken: From<TokenStruct<K>>,
    {
        SomeToken::from(TokenStruct {
            kind: std::marker::PhantomData::<K>,
            loc: Loc::empty(),
        })
    }

    #[rustfmt::skip]
    let tokens = vec![
        t(Begin),
            t(Ident), t(Semicolon),
            t(Let), t(Underscore), t(Colon), t(Ident), t(Equal),
                t(Tag), t(LParen),
                    t(Tag), t(Comma),
                    t(Ident), t(Comma),
                t(RParen),
            t(Semicolon),
        t(End),
        t(EOF),
    ];

    let mut parser = Parser::new(&tokens);
    let expr = crate::syntax::Expr::parse(&mut parser, &[SomeTokenKind::EOF]).unwrap();

    dbg!(expr);
}

#[test]
fn test2() {
    use kinds::*;

    fn t<K>(_: K) -> SomeToken
    where
        SomeToken: From<TokenStruct<K>>,
    {
        SomeToken::from(TokenStruct {
            kind: std::marker::PhantomData::<K>,
            loc: Loc::empty(),
        })
    }

    #[rustfmt::skip]
    let tokens = vec![
        t(LParen),
        t(LParen),
        t(LParen),
        // t(RParen),
        t(Semicolon),
        t(EOF),
    ];

    let mut parser = Parser::new(&tokens);
    let expr = A::parse(&mut parser, &[SomeTokenKind::EOF]).unwrap();

    dbg!(expr);
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[macro_rules_attribute::derive(ParseDerive!)]
struct A {
    pub a: Vec<Token!["("]>,
    pub b: Option<Token![")"]>,
    pub c: Token![";"],
}
