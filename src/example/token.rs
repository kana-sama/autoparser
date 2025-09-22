use crate::token::token_set;

pub use token::*;

token_set! {
    mod token {
        pub struct Set;

        pub mod kinds {
            "ident"             => Ident,
            "#tag"              => Tag,

            "("                 => LParen,
            ")"                 => RParen,

            "="                 => Equal,
            ";"                 => Semicolon,
            ":"                 => Colon,
            "_"                 => Underscore,
            "->"                => Arrow,
            ","                 => Comma,

            "begin"             => Begin,
            "end"               => End,
            "let"               => Let,
            "else"              => Else,
            "return"            => Return,
            "case"              => Case,
            "of"                => Of,
            "enum"              => Enum,
            "function"          => Function,
            "returns"           => Returns,
            "type"              => Type,
            "module"            => Module,
            "public"            => Public,
            "interface"         => Interface,
            "implementation"    => Implementation,
        }
    }
}
