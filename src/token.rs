use std::ops::Range;

#[derive(Clone, PartialEq, Eq)]
pub struct TokenOfKind<K> {
    pub kind: K,
    pub loc:  Range<usize>,
}

pub trait TokenSet: Clone {
    type Kind: KindExt;
    type Token: TokenExt;

    const EOF: Self::Kind;

    fn kind(token: &Self::Token) -> Self::Kind;
}

pub trait KindOfSet<Tok: TokenSet>: Clone + Sized + 'static {
    const KIND: Tok::Kind;
    const ALIAS: &str;

    fn from_token<'tok>(token: &'tok Tok::Token) -> Option<&'tok TokenOfKind<Self>>;
    fn to_token(token: TokenOfKind<Self>) -> Tok::Token;
}

pub trait KindExt: Clone + Copy + PartialEq + Eq + std::fmt::Debug {
    fn to_string(&self) -> &'static str;
}

pub trait TokenExt: Clone {
    fn loc(&self) -> Range<usize>;
    fn to_source_string<'source>(&self, source: &'source str) -> &'source str;
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
                EOF,
            }

            #[derive(Clone, PartialEq, Eq, Debug)]
            #[derive(derive_more::From)]
            pub enum Token {
                $( $Kind($crate::token::TokenOfKind<$kinds_mod::$Kind>), )*
                EOF($crate::token::TokenOfKind<$crate::token::EOF>),
            }

            impl $crate::token::KindExt for Kind {
                fn to_string(&self) -> &'static str {
                    match self {
                        $( Self::$Kind => $alias, )*
                        Self::EOF => "EOF",
                    }
                }
            }

            impl $crate::token::TokenExt for Token {
                fn loc(&self) -> std::ops::Range<usize> {
                    match self {
                        $( Self::$Kind(tok) => tok.loc.clone(), )*
                        Self::EOF(tok) => tok.loc.clone(),
                    }
                }

                fn to_source_string<'source>(&self, source: &'source str) -> &'source str {
                    let loc = self.loc();
                    &source[loc.start..loc.end]
                }
            }

            impl $crate::token::TokenSet for Set {
                type Kind = Kind;
                type Token = Token;

                const EOF: Self::Kind = Kind::EOF;

                fn kind(token: &Self::Token) -> Self::Kind {
                    match token {
                        $( Self::Token::$Kind(_) => Kind::$Kind, )*
                        Self::Token::EOF(_) => Kind::EOF,
                    }
                }
            }

            $kinds_vis mod $kinds_mod {
                $(
                    #[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
                    pub struct $Kind;

                    $crate::token::impl_kind_of_set!($Kind, $Kind, $alias);
                )*

                $crate::token::impl_kind_of_set!(EOF, $crate::token::EOF, "EOF");
            }

            macro_rules! K {
                $(
                    ( EOF    ) => { $crate::token::EOF };
                    ( $alias ) => { $mod::kinds::$Kind };
                )*
            }

            macro_rules! k {
                $(
                    ( EOF    ) => { $crate::token::EOF };
                    ( $alias ) => { $mod::kinds::$Kind };
                )*
            }

            macro_rules! T {
                $(
                    ( EOF    ) => { $crate::token::TokenOfKind<$crate::token::EOF> };
                    ( $alias ) => { $crate::token::TokenOfKind<$mod::kinds::$Kind> };
                )*
            }

            macro_rules! t {
                $(
                    ( EOF    ) => {
                        $crate::token::TokenOfKind {
                            kind: $crate::token::EOF,
                            loc: Default::default(),
                        }.into()
                    };

                    ( $alias ) => {
                        $crate::token::TokenOfKind {
                            kind: $mod::kinds::$Kind::default(),
                            loc: Default::default(),
                        }.into()
                    };
                )*
            }

            pub(crate) use { K, k, T, t };
        }
    };
}

#[allow(unused)]
macro_rules! impl_kind_of_set {
    ($K:ident, $KTy:ty, $a:tt) => {
        impl $crate::token::KindOfSet<super::Set> for $KTy {
            const KIND: <super::Set as $crate::token::TokenSet>::Kind = super::Kind::$K;
            const ALIAS: &str = $a;

            fn from_token<'tok>(
                token: &'tok <super::Set as $crate::token::TokenSet>::Token,
            ) -> Option<&'tok $crate::token::TokenOfKind<Self>> {
                if let super::Token::$K(tok) = token {
                    Some(tok)
                } else {
                    None
                }
            }

            fn to_token(
                token: $crate::token::TokenOfKind<Self>,
            ) -> <super::Set as $crate::token::TokenSet>::Token {
                super::Token::$K(token)
            }
        }
    };
}

#[allow(unused)]
pub(crate) use {impl_kind_of_set, token_set};

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct EOF;
