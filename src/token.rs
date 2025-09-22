#[derive(Clone, PartialEq, Eq)]
pub struct TokenOfKind<K> {
    pub kind: K,
    pub loc: Loc,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Loc {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

pub trait TokenSet {
    type Kind: Clone + Copy + PartialEq + Eq;
    type Token: Clone;

    fn kind(token: &Self::Token) -> Self::Kind;
}

pub trait KindOfSet<Tok: TokenSet>: Clone + Sized + 'static {
    const KIND: Tok::Kind;
    fn from_token<'tok>(token: &'tok Tok::Token) -> Option<&'tok TokenOfKind<Self>>;
}

impl<K: Default + std::fmt::Debug> std::fmt::Debug for TokenOfKind<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        K::fmt(&K::default(), f)
    }
}

#[allow(unused)]
macro_rules! token_set {
    (
        $vis:vis mod $mod:ident {
            pub struct Set;

            $kinds_vis:vis mod $kinds_mod:ident {
                $( $alias:tt => $Kind:ident, )*
            }
        }
    ) => {
        $vis mod $mod {
            #![allow(unused)]

            #[derive(Clone, Copy, PartialEq, Eq, Debug)]
            pub struct Set;

            #[derive(Clone, Copy, PartialEq, Eq, Debug)]
            pub enum Kind {
                $( $Kind, )*
            }

            #[derive(Clone, PartialEq, Eq, Debug)]
            #[derive(derive_more::From)]
            pub enum Token {
                $( $Kind($crate::token::TokenOfKind<$kinds_mod::$Kind>), )*
            }

            impl $crate::token::TokenSet for Set {
                type Kind = Kind;
                type Token = Token;

                fn kind(token: &Self::Token) -> Self::Kind {
                    match token {
                        $( Self::Token::$Kind(_) => Kind::$Kind, )*
                    }
                }
            }

            $kinds_vis mod $kinds_mod {
                $(
                    #[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
                    pub struct $Kind;

                    impl $crate::token::KindOfSet<super::Set> for $Kind {
                        const KIND: <super::Set as $crate::token::TokenSet>::Kind = super::Kind::$Kind;

                        fn from_token<'tok>(token: &'tok <super::Set as crate::token::TokenSet>::Token) -> Option<&'tok $crate::token::TokenOfKind<Self>> {
                            if let super::Token::$Kind(tok) = token {
                                Some(tok)
                            } else {
                                None
                            }
                        }
                    }
                )*
            }

            macro_rules! K {
                $(
                    ( $alias ) => {
                        $mod::kinds::$Kind
                    };
                )*
            }

            macro_rules! T {
                $(
                    ( $alias ) => {
                        $crate::token::TokenOfKind<$mod::kinds::$Kind>
                    };
                )*
            }

            macro_rules! t {
                $(
                    ( $alias ) => {
                        $crate::token::TokenOfKind {
                            kind: $mod::kinds::$Kind::default(),
                            loc: Default::default(),
                        }.into()
                    };
                )*
            }

            pub(crate) use { K, T, t };
        }
    };
}

#[allow(unused)]
pub(crate) use token_set;
