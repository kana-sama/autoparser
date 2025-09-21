macro_rules! tokens {
    (
        $some_token_vis:vis enum $some_token:ident;
        $some_kind_vis:vis enum $some_kind:ident;

        macro $token_macro:ident! {
            $( $str:tt => $Kind:ident $type:tt, )*
        }
    ) => {
        pub mod kinds {
            $(
                #[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
                pub struct $Kind;
            )*
        }

        $( tokens!(impl KindTrait for $Kind($str) with $type); )*

        #[derive(Debug, Clone, PartialEq, Eq, derive_more::From, derive_more::TryInto)]
        #[try_into(owned, ref, ref_mut)]
        $some_token_vis enum $some_token {
            $( $Kind(TokenStruct<kinds::$Kind>), )*
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        $some_kind_vis enum $some_kind {
            $( $Kind, )*
        }

        $(
            impl TokenTrait for TokenStruct<kinds::$Kind> {
                const KIND: $some_kind = SomeTokenKind::$Kind;
            }
        )*

        impl SomeToken {
            pub fn kind(&self) -> SomeTokenKind {
                match self {
                    $( SomeToken::$Kind(_) => SomeTokenKind::$Kind, )*
                }
            }
        }

        macro_rules! $token_macro {
            $(
                [$str] => {
                    $crate::token::TokenStruct::<$crate::token::kinds::$Kind>
                };
            )*
        }

        #[allow(unused)]
        pub(crate) use Token;

    };

    (impl KindTrait for $Kind:ident($str:literal) with delimiter) => {
        impl KindTrait for TokenStruct<kinds::$Kind> {
            const IS_KEYWORD: bool = false;
            fn as_string(&self, _: &str) -> &'static str {
                $str
            }
        }
    };

    (impl KindTrait for $Kind:ident($str:literal) with keyword) => {
        impl KindTrait for TokenStruct<kinds::$Kind> {
            const IS_KEYWORD: bool = true;
            fn as_string(&self, _: &str) -> &'static str {
                $str
            }
        }
    };

    (impl KindTrait for $Kind:ident($str:literal) with capture) => {
        impl KindTrait for TokenStruct<kinds::$Kind> {
            const IS_KEYWORD: bool = false;
            fn as_string<'source>(&self, source: &'source str) -> &'source str {
                &source[self.loc.start..self.loc.end]
            }
        }
    };
}

pub(crate) use tokens;

#[derive(Clone, PartialEq, Eq)]
pub struct TokenStruct<K> {
    pub kind: std::marker::PhantomData<K>,
    pub loc: Loc,
}

impl<K: Default + std::fmt::Debug> std::fmt::Debug for TokenStruct<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        K::fmt(&K::default(), f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Loc {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl Loc {
    pub fn empty() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 0,
            column: 0,
        }
    }
}

pub trait TokenTrait {
    const KIND: SomeTokenKind;
}

pub trait KindTrait {
    const IS_KEYWORD: bool;
    fn as_string<'source>(&self, source: &'source str) -> &'source str;
}

tokens! {
    pub enum SomeToken;
    pub enum SomeTokenKind;

    macro Token! {
        "ident"             => Ident            capture,
        "#tag"              => Tag              capture,

        "("                 => LParen           delimiter,
        ")"                 => RParen           delimiter,
        "{"                 => LBrace           delimiter,
        "}"                 => RBrace           delimiter,
        "["                 => LBracket         delimiter,
        "]"                 => RBracket         delimiter,

        "="                 => Equal            delimiter,
        ";"                 => Semicolon        delimiter,
        ":"                 => Colon            delimiter,
        "_"                 => Underscore       delimiter,
        "->"                => Arrow            delimiter,
        ","                 => Comma            delimiter,
        "EOF"               => EOF              delimiter,

        "begin"             => Begin            keyword,
        "end"               => End              keyword,
        "let"               => Let              keyword,
        "else"              => Else             keyword,
        "return"            => Return           keyword,
        "case"              => Case             keyword,
        "of"                => Of               keyword,
        "enum"              => Enum             keyword,
        "function"          => Function         keyword,
        "returns"           => Returns          keyword,
        "type"              => Type             keyword,
        "module"            => Module           keyword,
        "public"            => Public           keyword,
        "interface"         => Interface        keyword,
        "implementation"    => Implementation   keyword,
    }
}
