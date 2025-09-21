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
    const FIRST_TOKENS: &[SomeTokenKind];
    fn parse(parser: &mut Parser, until_tokens: &[SomeTokenKind]) -> Result<Self, ParseError>;
}

impl<K: Clone + 'static> Parse for TokenStruct<K>
where
    TokenStruct<K>: TokenTrait,
    for<'a> &'a TokenStruct<K>: TryFrom<&'a SomeToken>,
{
    const FIRST_TOKENS: &[SomeTokenKind] = &[TokenStruct::<K>::KIND];

    fn parse(parser: &mut Parser, _until_tokens: &[SomeTokenKind]) -> Result<Self, ParseError> {
        let token = parser.take::<Self>()?;
        return Ok(token.clone());
    }
}

impl<T: Parse> Parse for Option<T> {
    const FIRST_TOKENS: &[SomeTokenKind] = T::FIRST_TOKENS;

    fn parse(parser: &mut Parser, until_tokens: &[SomeTokenKind]) -> Result<Self, ParseError> {
        let node = if parser.peek_one_of(Self::FIRST_TOKENS).is_some() {
            Some(T::parse(parser, until_tokens)?)
        } else {
            None
        };

        return Ok(node);
    }
}

impl<T: Parse> Parse for Vec<T> {
    const FIRST_TOKENS: &[SomeTokenKind] = T::FIRST_TOKENS;

    fn parse(parser: &mut Parser, until_tokens: &[SomeTokenKind]) -> Result<Self, ParseError> {
        let mut nodes = Vec::new();

        while parser.peek_one_of(until_tokens).is_none() {
            nodes.push(T::parse(parser, until_tokens)?);
        }

        return Ok(nodes);
    }
}

impl<T: Parse, G: Parse> Parse for (T, G) {
    const FIRST_TOKENS: &[SomeTokenKind] = T::FIRST_TOKENS;

    fn parse(parser: &mut Parser, until_tokens: &[SomeTokenKind]) -> Result<Self, ParseError> {
        return Ok((
            T::parse(parser, G::FIRST_TOKENS)?,
            G::parse(parser, until_tokens)?,
        ));
    }
}

impl<T: Parse> Parse for Box<T> {
    const FIRST_TOKENS: &[SomeTokenKind] = T::FIRST_TOKENS;

    fn parse(parser: &mut Parser, until_tokens: &[SomeTokenKind]) -> Result<Self, ParseError> {
        return Ok(Box::new(T::parse(parser, until_tokens)?));
    }
}

macro_rules! ParseDerive {
    (
        $( #[$_1:meta] )*
        $vis:vis struct $name:ident {
            $( #[$_2:meta] )*
            $_3:vis $first_field_name:ident : $first_field_type:ty,
            $( $( #[$_4:meta] )* $_5:vis $field_name:ident : $field_type:ty, )*
        }
    ) => {
        impl $crate::parser::Parse for $name {
            const FIRST_TOKENS: &[crate::token::SomeTokenKind] = <$first_field_type>::FIRST_TOKENS;

            fn parse(parser: &mut $crate::parser::Parser, until_tokens: &[crate::token::SomeTokenKind]) -> Result<Self, $crate::parser::ParseError> {
                ParseDerive! { field parser until_tokens [ $first_field_name : $first_field_type, $( $field_name : $field_type, )* ] }

                return Ok(Self {
                    $first_field_name,
                    $( $field_name, )*
                });
            }
        }
    };

    (field $parser:ident $until_tokens:ident [$i1:ident : $t1:ty, $i2:ident : $t2:ty, $( $in:ident : $tn:ty, )*]) => {
        let $i1 = $crate::parser::Parse::parse($parser, <$t2>::FIRST_TOKENS)?;
        ParseDerive!(field $parser $until_tokens [ $i2 : $t2, $( $in : $tn, )* ])
    };

    (field $parser:ident $until_tokens:ident [$i1:ident : $t1:ty,]) => {
        let $i1 = $crate::parser::Parse::parse($parser, $until_tokens)?;
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
            const FIRST_TOKENS: &[crate::token::SomeTokenKind] = constcat::concat_slices!([crate::token::SomeTokenKind]:
                $( <$ty>::FIRST_TOKENS, )*
            );

            fn parse(parser: &mut $crate::parser::Parser, until_tokens: &[crate::token::SomeTokenKind]) -> Result<Self, $crate::parser::ParseError> {
                $(
                    if parser.peek_one_of(<$ty>::FIRST_TOKENS).is_some() {
                        return Ok($name::$arm($crate::parser::Parse::parse(parser, until_tokens)?));
                    }
                )*

                return Err($crate::parser::ParseError::UnexpectedToken {
                    expected: Self::FIRST_TOKENS.to_vec(),
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
            t(Let), t(Ident), t(Colon), t(Ident), t(Equal),
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
