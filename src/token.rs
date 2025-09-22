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

impl Loc {
    pub fn range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

pub trait TokenSet {
    type Kind: Clone + Copy + PartialEq + Eq;
    type Token: Clone;

    fn kind(token: &Self::Token) -> Self::Kind;
}

pub trait KindOfSet<Tok: TokenSet>: Clone + Sized + 'static {
    const KIND: Tok::Kind;
    const ALIAS: &str;

    fn from_token<'tok>(token: &'tok Tok::Token) -> Option<&'tok TokenOfKind<Self>>;
    fn to_token(token: TokenOfKind<Self>) -> Tok::Token;
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

            impl Kind {
                pub fn to_string(&self) -> &'static str {
                    match self {
                        $( Self::$Kind => $alias, )*
                    }
                }
            }

            impl Token {
                pub fn loc(&self) -> &crate::token::Loc {
                    match self {
                        $( Self::$Kind(tok) => &tok.loc, )*
                    }
                }

                pub fn to_source_string<'source>(&self, source: &'source str) -> &'source str {
                    let loc = self.loc();
                    &source[loc.start..loc.end]
                }
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
                        const ALIAS: &str = $alias;

                        fn from_token<'tok>(token: &'tok <super::Set as crate::token::TokenSet>::Token) -> Option<&'tok $crate::token::TokenOfKind<Self>> {
                            if let super::Token::$Kind(tok) = token {
                                Some(tok)
                            } else {
                                None
                            }
                        }

                        fn to_token(token: $crate::token::TokenOfKind<Self>) -> <super::Set as $crate::token::TokenSet>::Token {
                            super::Token::$Kind(token)
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

            macro_rules! k {
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

            pub(crate) use { K, k, T, t };
        }
    };
}

#[allow(unused)]
pub(crate) use token_set;
