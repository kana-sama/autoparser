use crate::parser::*;
use crate::token::*;

#[derive(Clone, PartialEq, Eq)]
pub enum Separated<T, Sep> {
    Empty,
    NonEmpty(NonEmptySeparated<T, Sep>),
}

#[derive(Clone, PartialEq, Eq)]
pub struct NonEmptySeparated<T, Sep> {
    pub first: T,
    pub rest: Option<Box<NonEmptySeparatedRest<T, Sep>>>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct NonEmptySeparatedRest<T, Sep> {
    pub sep: Sep,
    pub rest: Option<(T, Option<Box<NonEmptySeparatedRest<T, Sep>>>)>,
}

trait DebugList {
    fn fmt_items(&self, f: &mut std::fmt::DebugList);
}

impl<T: std::fmt::Debug, Sep: std::fmt::Debug> std::fmt::Debug for Separated<T, Sep> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Separated")
            .field_with(|f| {
                let mut list = f.debug_list();
                self.fmt_items(&mut list);
                list.finish()
            })
            .finish()
    }
}

impl<T: std::fmt::Debug, Sep: std::fmt::Debug> std::fmt::Debug for NonEmptySeparated<T, Sep> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NonEmptySeparated")
            .field_with(|f| {
                let mut list = f.debug_list();
                self.fmt_items(&mut list);
                list.finish()
            })
            .finish()
    }
}

impl<T: std::fmt::Debug, Sep: std::fmt::Debug> DebugList for Separated<T, Sep> {
    fn fmt_items(&self, list: &mut std::fmt::DebugList) {
        match self {
            Separated::Empty => {}
            Separated::NonEmpty(non_empty) => {
                non_empty.fmt_items(list);
            }
        }
    }
}

impl<T: std::fmt::Debug, Sep: std::fmt::Debug> DebugList for NonEmptySeparated<T, Sep> {
    fn fmt_items(&self, list: &mut std::fmt::DebugList) {
        list.entry(&self.first);

        let Some(rest) = &self.rest else {
            return;
        };

        rest.fmt_items(list);
    }
}

impl<T: std::fmt::Debug, Sep: std::fmt::Debug> DebugList for NonEmptySeparatedRest<T, Sep> {
    fn fmt_items(&self, list: &mut std::fmt::DebugList) {
        list.entry(&self.sep);

        let Some((item, next)) = &self.rest else {
            return;
        };

        list.entry(item);

        let Some(next) = next else {
            return;
        };

        next.fmt_items(list);
    }
}

impl<T: Parse, Sep: Parse> Parse for NonEmptySeparated<T, Sep>
where
    Sep: TokenTrait + Clone + 'static,
    for<'a> &'a Sep: TryFrom<&'a SomeToken>,
{
    const FIRST_TOKENS: &[SomeTokenKind] = T::FIRST_TOKENS;

    fn parse(parser: &mut Parser, until_tokens: &[SomeTokenKind]) -> Result<Self, ParseError> {
        let first = T::parse(parser, &[until_tokens, Sep::FIRST_TOKENS].concat())?;

        if parser.peek_one_of(until_tokens).is_some() {
            return Ok(NonEmptySeparated { first, rest: None });
        }

        if parser.peek::<Sep>().is_some() {
            let rest = Parse::parse(parser, until_tokens)?;
            return Ok(NonEmptySeparated {
                first,
                rest: Some(rest),
            });
        }

        return Err(ParseError::UnexpectedToken {
            expected: [until_tokens, Sep::FIRST_TOKENS].concat(),
            got: parser.peek_some().map(Clone::clone),
        });
    }
}

impl<T: Parse, Sep: Parse> Parse for NonEmptySeparatedRest<T, Sep>
where
    Sep: TokenTrait + Clone + 'static,
    for<'a> &'a Sep: TryFrom<&'a SomeToken>,
{
    const FIRST_TOKENS: &[SomeTokenKind] = &[Sep::KIND];

    fn parse(parser: &mut Parser, until_tokens: &[SomeTokenKind]) -> Result<Self, ParseError> {
        let sep = parser.take::<Sep>()?.clone();

        if parser.peek_one_of(until_tokens).is_some() {
            return Ok(NonEmptySeparatedRest { sep, rest: None });
        }

        if parser.peek_one_of(T::FIRST_TOKENS).is_some() {
            let t = T::parse(parser, &[until_tokens, Sep::FIRST_TOKENS].concat())?;

            if parser.peek_one_of(until_tokens).is_some() {
                return Ok(NonEmptySeparatedRest {
                    sep,
                    rest: Some((t, None)),
                });
            }

            if parser.peek::<Sep>().is_some() {
                let rest = Parse::parse(parser, until_tokens)?;
                return Ok(NonEmptySeparatedRest {
                    sep,
                    rest: Some((t, Some(Box::new(rest)))),
                });
            }
        }

        return Err(ParseError::UnexpectedToken {
            expected: [until_tokens, T::FIRST_TOKENS].concat(),
            got: parser.peek_some().map(Clone::clone),
        });
    }
}

impl<T: Parse, Sep: Parse> Parse for Separated<T, Sep>
where
    Sep: TokenTrait + Clone + 'static,
    for<'a> &'a Sep: TryFrom<&'a SomeToken>,
{
    const FIRST_TOKENS: &[SomeTokenKind] = T::FIRST_TOKENS;

    fn parse(parser: &mut Parser, until_tokens: &[SomeTokenKind]) -> Result<Self, ParseError> {
        if parser.peek_one_of(Self::FIRST_TOKENS).is_some() {
            return Ok(Separated::NonEmpty(Parse::parse(parser, until_tokens)?));
        }

        return Ok(Separated::Empty);
    }
}
