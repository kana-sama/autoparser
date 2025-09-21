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
