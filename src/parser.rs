use crate::separated::*;
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

    pub fn peek_one_of<'a>(&self, kinds: &'a [SomeTokenKind]) -> Option<&'tok SomeToken> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenSet {
    pub tokens: Vec<SomeTokenKind>,
    pub can_be_empty: bool,
}

impl TokenSet {
    pub fn new(tokens: Vec<SomeTokenKind>) -> Self {
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

pub type First = TokenSet;

pub trait Parse: Sized {
    fn first() -> First;
    fn parse(parser: &mut Parser) -> Result<Self, ParseError>;
}

impl<K: Clone + 'static> Parse for TokenStruct<K>
where
    TokenStruct<K>: TokenTrait,
    for<'a> &'a TokenStruct<K>: TryFrom<&'a SomeToken>,
{
    fn first() -> First {
        First::new(vec![TokenStruct::<K>::KIND])
    }

    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let token = parser.take::<Self>()?;
        return Ok(token.clone());
    }
}

impl<T: Parse> Parse for Option<T> {
    fn first() -> First {
        T::first().optional()
    }

    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let node = if parser.peek_one_of(&Self::first().tokens).is_some() {
            Some(T::parse(parser)?)
        } else {
            None
        };

        return Ok(node);
    }
}

impl<T: Parse> Parse for Vec<T> {
    fn first() -> First {
        T::first().optional()
    }

    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let mut nodes = Vec::new();

        while parser.peek_one_of(&Self::first().tokens).is_some() {
            nodes.push(T::parse(parser)?);
        }

        return Ok(nodes);
    }
}

impl<T: Parse, G: Parse> Parse for (T, G) {
    fn first() -> First {
        T::first().follow(G::first)
    }

    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        return Ok((T::parse(parser)?, G::parse(parser)?));
    }
}

impl<T: Parse> Parse for Box<T> {
    fn first() -> First {
        T::first()
    }

    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        return Ok(Box::new(T::parse(parser)?));
    }
}

impl<T: Parse, Sep: Parse> Parse for NonEmptySeparated<T, Sep>
where
    Sep: TokenTrait + Clone + 'static,
    for<'a> &'a Sep: TryFrom<&'a SomeToken>,
{
    fn first() -> First {
        T::first()
    }

    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let first = T::parse(parser)?;
        let rest = Option::parse(parser)?;
        return Ok(NonEmptySeparated { first, rest });
    }
}

impl<T: Parse, Sep: Parse> Parse for NonEmptySeparatedRest<T, Sep>
where
    Sep: TokenTrait + Clone + 'static,
    for<'a> &'a Sep: TryFrom<&'a SomeToken>,
{
    fn first() -> First {
        Sep::first()
    }

    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let sep = parser.take::<Sep>()?.clone();

        if parser.peek_one_of(&T::first().tokens).is_some() {
            let t = T::parse(parser)?;

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
            expected: T::first().tokens,
            got: parser.peek_some().map(Clone::clone),
        });
    }
}

impl<T: Parse, Sep: Parse> Parse for Separated<T, Sep>
where
    Sep: TokenTrait + Clone + 'static,
    for<'a> &'a Sep: TryFrom<&'a SomeToken>,
{
    fn first() -> First {
        T::first().optional()
    }

    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        if parser.peek_one_of(&Self::first().tokens).is_some() {
            return Ok(Separated::NonEmpty(Parse::parse(parser)?));
        }

        return Ok(Separated::Empty);
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
            fn first() -> $crate::parser::First {
                <$t1>::first() $( .follow( <$tn>::first ) )*
            }

            fn parse(parser: &mut $crate::parser::Parser) -> Result<Self, $crate::parser::ParseError> {
                ParseDerive! { field parser [ $i1 : $t1, $( $in : $tn, )* ] }

                return Ok(Self {
                    $i1,
                    $( $in, )*
                });
            }
        }
    };

    (field $parser:ident [$i1:ident : $t1:ty, $i2:ident : $t2:ty, $( $in:ident : $tn:ty, )*]) => {
        let $i1 = $crate::parser::Parse::parse($parser)?;
        ParseDerive!(field $parser [ $i2 : $t2, $( $in : $tn, )* ])
    };

    (field $parser:ident [$i1:ident : $t1:ty,]) => {
        let $i1 = $crate::parser::Parse::parse($parser)?;
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
            fn first() -> $crate::parser::First {
                let mut tokens = Vec::new();
                $(
                    tokens.extend(<$ty>::first().tokens);
                )*

                $crate::parser::First::new(tokens)
            }

            fn parse(parser: &mut $crate::parser::Parser) -> Result<Self, $crate::parser::ParseError> {
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

pub(crate) use ParseDerive;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr() {
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

        let mut parser = Parser::new(&tokens);
        let expr = crate::syntax::Expr::parse(&mut parser).unwrap();

        dbg!(expr);
    }

    #[test]
    fn test_follow_optional() {
        // S -> A B ';'
        //
        // A ->
        // A -> '(' A
        //
        // B ->
        // B -> ')' B

        // без рекурсивного проброса FOLLOW, A будет ждать ")" для завершения
        // но на самом деле там может быть как ")", так и ";", потому что ")" - это опциональный токен
        // поэтому нужно рекурсивно к FOLLOW(T) прибавлять FIRST следующих нонтерминалов, если T может быть пустым

        #[derive(Debug, Clone, PartialEq, Eq)]
        #[macro_rules_attribute::derive(ParseDerive!)]
        struct A {
            pub a: Vec<Token!["("]>,
            pub b: Vec<Token![")"]>,
            pub eof: Token![";"],
        }

        let tokens = vec![t!["("], t![";"], t!["EOF"]];
        let mut parser = Parser::new(&tokens);
        let expr = A::parse(&mut parser).unwrap();
        assert!(expr.a.len() == 1);
        assert!(expr.b.len() == 0);

        let tokens = vec![t!["("], t![")"], t![";"], t!["EOF"]];
        let mut parser = Parser::new(&tokens);
        let expr = A::parse(&mut parser).unwrap();
        assert!(expr.a.len() == 1);
        assert!(expr.b.len() == 1);

        dbg!("done");
    }

    #[test]
    pub fn test_follow_is_necessary() {
        // https://stackoverflow.com/a/70118055
        // тут утверждается что FOLLOW нужен для LL(1) в общем случае
        // у меня есть интуиция что это не так, но нужно проверить

        // мое предположение что FOLLOW нужен только для того чтобы упать с ошибкой
        // в нужном месте, а не где-то после

        // вот грамматика из ответа, которую я и протестирую

        // S -> A
        // S -> C

        // A -> B '('
        // B -> '['
        // B ->

        // C -> D ')'
        // D -> ']'
        // D ->

        #[derive(Debug, Clone, PartialEq, Eq)]
        #[macro_rules_attribute::derive(ParseDerive!)]
        enum S {
            A((Option<Token!["["]>, Token!["("])),
            B((Option<Token!["]"]>, Token![")"])),
        }

        let tokens = vec![t!["["], t!["("], t!["]"], t![")"], t!["EOF"]];
        let mut parser = Parser::new(&tokens);
        _ = S::parse(&mut parser).unwrap();

        let tokens = vec![t!["["], t!["("], t![")"], t!["EOF"]];
        let mut parser = Parser::new(&tokens);
        _ = S::parse(&mut parser).unwrap();

        let tokens = vec![t!["("], t![")"], t!["EOF"]];
        let mut parser = Parser::new(&tokens);
        _ = S::parse(&mut parser).unwrap();

        let tokens = vec![t!["("], t!["]"], t![")"], t!["EOF"]];
        let mut parser = Parser::new(&tokens);
        _ = S::parse(&mut parser).unwrap();

        dbg!("done");
    }
}
