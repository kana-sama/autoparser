use crate::token::TokenOfKind;
use std::fmt::{Debug, DebugList};

#[derive(Clone, PartialEq, Eq)]
pub enum Separated<T, Sep> {
    Empty,
    NonEmpty(NonEmptySeparated<T, Sep>),
}

#[derive(Clone, PartialEq, Eq)]
pub struct NonEmptySeparated<T, Sep> {
    pub first:    T,
    pub rest:     Vec<(TokenOfKind<Sep>, T)>,
    pub trailing: Option<TokenOfKind<Sep>>,
}

trait DebugAsList {
    fn fmt_items(&self, f: &mut DebugList);
}

fn debug_from_list(
    name: &str,
    items: &impl DebugAsList,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    f.debug_tuple(name)
        .field_with(|f| {
            let mut list = f.debug_list();
            items.fmt_items(&mut list);
            list.finish()
        })
        .finish()
}

impl<T: Debug, Sep: Default + Debug> Debug for Separated<T, Sep> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug_from_list("Separated", self, f)
    }
}

impl<T: Debug, Sep: Default + Debug> Debug for NonEmptySeparated<T, Sep> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug_from_list("NonEmptySeparated", self, f)
    }
}

impl<T: Debug, Sep: Default + Debug> DebugAsList for Separated<T, Sep> {
    fn fmt_items(&self, list: &mut DebugList) {
        if let Separated::NonEmpty(non_empty) = self {
            non_empty.fmt_items(list);
        }
    }
}

impl<T: Debug, Sep: Default + Debug> DebugAsList for NonEmptySeparated<T, Sep> {
    fn fmt_items(&self, list: &mut DebugList) {
        list.entry(&self.first);

        for (sep, item) in &self.rest {
            list.entry(sep);
            list.entry(item);
        }

        if let Some(trailing) = &self.trailing {
            list.entry(trailing);
        }
    }
}
