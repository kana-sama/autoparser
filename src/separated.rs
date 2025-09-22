use crate::token::TokenOfKind;
use std::fmt::{Debug, DebugList};

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
    pub sep: TokenOfKind<Sep>,
    pub rest: Option<(T, Option<Box<NonEmptySeparatedRest<T, Sep>>>)>,
}

trait DebugAsList {
    fn fmt_items(&self, f: &mut DebugList);
}

impl<T: Debug, Sep: Debug> Debug for Separated<T, Sep>
where
    TokenOfKind<Sep>: Debug,
{
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

impl<T: Debug, Sep: Debug> Debug for NonEmptySeparated<T, Sep>
where
    TokenOfKind<Sep>: Debug,
{
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

impl<T: Debug, Sep: Debug> DebugAsList for Separated<T, Sep>
where
    TokenOfKind<Sep>: Debug,
{
    fn fmt_items(&self, list: &mut DebugList) {
        match self {
            Separated::Empty => {}
            Separated::NonEmpty(non_empty) => {
                non_empty.fmt_items(list);
            }
        }
    }
}

impl<T: Debug, Sep: Debug> DebugAsList for NonEmptySeparated<T, Sep>
where
    TokenOfKind<Sep>: Debug,
{
    fn fmt_items(&self, list: &mut DebugList) {
        list.entry(&self.first);

        let Some(rest) = &self.rest else {
            return;
        };

        rest.fmt_items(list);
    }
}

impl<T: Debug, Sep> DebugAsList for NonEmptySeparatedRest<T, Sep>
where
    TokenOfKind<Sep>: Debug,
{
    fn fmt_items(&self, list: &mut DebugList) {
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
