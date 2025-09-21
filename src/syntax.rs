use macro_rules_attribute::{apply, attribute_alias};

use crate::parser::ParseDerive;
use crate::separated::{NonEmptySeparated, Separated};
use crate::token::*;

attribute_alias! {
    #[apply(node)] =
        #[macro_rules_attribute::derive(ParseDerive!)]
        #[apply(unpacked_debug)]
        #[derive(Clone, PartialEq, Eq)];
}

macro_rules! unpacked_debug {
    (
        $( #[$meta:meta] )*
        $vis:vis enum $name:ident {
            $( $con:ident( $ty:ty ), )*
        }
    ) => {
        $( #[$meta] )*
        #[derive(derive_more::Debug)]
        $vis enum $name {
            $( #[debug("{_0:?}")] $con($ty), )*
        }
    };

    (
        $( #[$meta:meta] )*
        $vis:vis struct $name:ident {
            $( $field_vis:vis $field:ident : $ty:ty, )*
        }
    ) => {
        $( #[$meta] )*
        #[derive(derive_more::Debug)]
        $vis struct $name {
            $( $field_vis $field: $ty, )*
        }
    };
}

#[apply(node)]
pub enum Expr {
    Ident(expr::Ident),
    Variant(expr::Variant),
    Let(expr::Let),
    Block(expr::Block),
    Return(expr::Return),
    Case(expr::Case),
    Annotated(expr::Annotated),
}

pub mod expr {
    use super::*;

    pub type Ident = Token!["ident"];

    #[apply(node)]
    pub struct Variant {
        pub tag: Token!["#tag"],
        pub payload: Option<variant::Payload>,
    }

    pub mod variant {
        use super::*;

        #[apply(node)]
        pub struct Payload {
            pub l_paren: Token!["("],
            pub exprs: Separated<Box<Expr>, Token![","]>,
            pub r_paren: Token![")"],
        }
    }

    #[apply(node)]
    pub struct Let {
        pub r#let: Token!["let"],
        pub pat: Box<Pat>,
        pub type_ann: Option<r#let::TypeAnn>,
        pub eq: Token!["="],
        pub value: Box<Expr>,
        pub r#else: Option<r#let::Else>,
    }

    pub mod r#let {
        use super::*;

        #[apply(node)]
        pub struct TypeAnn {
            pub colon: Token![":"],
            pub r#type: Box<Type>,
        }

        #[apply(node)]
        pub struct Else {
            pub r#else: Token!["else"],
            pub expr: Box<Expr>,
        }
    }

    #[apply(node)]
    pub struct Block {
        pub begin: Token!["begin"],
        pub exprs: NonEmptySeparated<Box<Expr>, Token![";"]>,
        pub end: Token!["end"],
    }

    #[apply(node)]
    pub struct Return {
        pub r#return: Token!["return"],
        pub expr: Box<Expr>,
    }

    #[apply(node)]
    pub struct Case {
        pub case: Token!["case"],
        pub value: Box<Expr>,
        pub of: Token!["of"],
        pub arms: Separated<case::Arm, Token![";"]>,
        pub end: Token!["end"],
    }

    pub mod case {
        use super::*;

        #[apply(node)]
        pub struct Arm {
            pub pat: Box<Pat>,
            pub arrow: Token!["->"],
            pub body: Box<Expr>,
        }
    }

    #[apply(node)]
    pub struct Annotated {
        pub l_paren: Token!["("],
        pub expr: Box<Expr>,
        pub colon: Token![":"],
        pub r#type: Box<Type>,
        pub r_paren: Token![")"],
    }
}

#[apply(node)]
pub enum Pat {
    Wildcard(pat::Wildcard),
    Ident(pat::Ident),
    Variant(pat::Variant),
}

pub mod pat {
    use super::*;

    pub type Wildcard = Token!["_"];

    pub type Ident = Token!["ident"];

    #[apply(node)]
    pub struct Variant {
        pub tag: Token!["#tag"],
        pub payload: Option<variant::Payload>,
    }

    pub mod variant {
        use super::*;

        #[apply(node)]
        pub struct Payload {
            pub l_paren: Token!["("],
            pub pats: Separated<Box<Pat>, Token![","]>,
            pub r_paren: Token![")"],
        }
    }
}

#[apply(node)]
pub enum Type {
    Ident(r#type::Ident),
    Enum(r#type::Enum),
    Function(r#type::Function),
}

pub mod r#type {
    use super::*;

    pub type Ident = Token!["ident"];

    #[apply(node)]
    pub struct Enum {
        pub l_bracket_token: Token!["enum"],
        pub variants: Separated<r#enum::Variant, Token![";"]>,
        pub r_bracket_token: Token!["end"],
    }

    pub mod r#enum {
        use super::*;

        #[apply(node)]
        pub struct Variant {
            pub tag: Token!["#tag"],
            pub payload: Option<variant::Payload>,
        }

        pub mod variant {
            use super::*;

            #[apply(node)]
            pub struct Payload {
                pub l_paren_token: Token!["("],
                pub types: Separated<Box<Type>, Token![","]>,
                pub r_paren_token: Token![")"],
            }
        }
    }

    #[apply(node)]
    pub struct Function {
        pub function_token: Token!["function"],
        pub l_paren_token: Token!["("],
        pub params: Separated<function::Param, Token![","]>,
        pub r_paren_token: Token![")"],
        pub arrow_token: Token!["returns"],
        pub return_type: Box<Type>,
    }

    pub mod function {
        use super::*;

        #[apply(node)]
        pub struct Param {
            pub name: Token!["ident"],
            pub colon_token: Token![":"],
            pub r#type: Box<Type>,
        }
    }
}

#[apply(node)]
pub struct Module {
    pub module: Token!["module"],
    pub interface: Option<module::Interface>,
    pub implementation: Option<module::Implementation>,
    pub end: Token!["end"],
}

pub mod module {
    use super::*;

    #[apply(node)]
    pub struct Interface {
        pub interface: Token!["interface"],
        pub items: Vec<InterfaceItem>,
    }

    #[apply(node)]
    pub struct Implementation {
        pub implementation: Token!["implementation"],
        pub items: Vec<ImplementationItem>,
    }

    #[apply(node)]
    pub enum InterfaceItem {
        TypeAlias(interface_item::TypeAlias),
        Function(interface_item::Function),
    }

    pub mod interface_item {
        use super::*;

        #[apply(node)]
        pub struct TypeAlias {
            pub r#type: Token!["type"],
            pub name: Token!["ident"],
            pub value: Option<type_alias::Value>,
            pub semicolon: Token![";"],
        }

        pub mod type_alias {
            use super::*;

            #[apply(node)]
            pub struct Value {
                pub eq_token: Token!["="],
                pub r#type: Box<Type>,
            }
        }

        #[apply(node)]
        pub struct Function {
            pub function: Token!["function"],
            pub name: Token!["ident"],
            pub l_paren: Token!["("],
            pub params: Separated<function::Param, Token![","]>,
            pub r_paren: Token![")"],
            pub returns: Token!["returns"],
            pub return_name: Token!["ident"],
            pub return_colon: Token![":"],
            pub return_type: Box<Type>,
            pub semicolon: Token![";"],
        }

        pub mod function {
            use super::*;

            #[apply(node)]
            pub struct Param {
                pub name: Token!["ident"],
                pub colon_token: Token![":"],
                pub r#type: Box<Type>,
            }
        }
    }

    #[apply(node)]
    pub enum ImplementationItem {
        TypeAlias(implementation_item::TypeAlias),
        Function(implementation_item::Function),
    }

    pub mod implementation_item {
        use super::*;

        #[apply(node)]
        pub struct TypeAlias {
            pub r#type: Token!["type"],
            pub name: Token!["ident"],
            pub eq: Token!["="],
            pub value: Box<Type>,
            pub semicolon_token: Token![";"],
        }

        #[apply(node)]
        pub struct Function {
            pub function_token: Token!["function"],
            pub name: Token!["ident"],
            pub l_paren_token: Token!["("],
            pub params: Separated<function::Param, Token![","]>,
            pub r_paren_token: Token![")"],
            pub returns: Token!["returns"],
            pub return_name: Token!["ident"],
            pub return_colon: Token![":"],
            pub return_type: Box<Type>,
            pub body: expr::Block,
        }

        pub mod function {
            use super::*;

            #[apply(node)]
            pub struct Param {
                pub name: Token!["ident"],
                pub colon_token: Token![":"],
                pub r#type: Box<Type>,
            }
        }
    }
}
