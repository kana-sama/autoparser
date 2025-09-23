use macro_rules_attribute::{apply, attribute_alias};

use crate::parser::derive_parse;
use crate::separated::{NonEmptySeparated, Separated};

use super::token;

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

    pub type Ident = token::T!["ident"];

    #[apply(node)]
    pub struct Variant {
        pub tag:     token::T!["#tag"],
        pub payload: Option<variant::Payload>,
    }

    pub mod variant {
        use super::*;

        #[apply(node)]
        pub struct Payload {
            l_paren:   token::T!["("],
            pub exprs: Separated<Box<Expr>, token::K![","]>,
            r_paren:   token::T![")"],
        }
    }

    #[apply(node)]
    pub struct Let {
        r#let:        token::T!["let"],
        pub pat:      Box<Pat>,
        pub type_ann: Option<r#let::TypeAnn>,
        eq:           token::T!["="],
        pub value:    Box<Expr>,
        pub r#else:   Option<r#let::Else>,
    }

    pub mod r#let {
        use super::*;

        #[apply(node)]
        pub struct TypeAnn {
            colon:      token::T![":"],
            pub r#type: Box<Type>,
        }

        #[apply(node)]
        pub struct Else {
            r#else:   token::T!["else"],
            pub expr: Box<Expr>,
        }
    }

    #[apply(node)]
    pub struct Block {
        begin:     token::T!["begin"],
        pub exprs: NonEmptySeparated<Box<Expr>, token::K![";"]>,
        end:       token::T!["end"],
    }

    #[apply(node)]
    pub struct Return {
        r#return: token::T!["return"],
        pub expr: Box<Expr>,
    }

    #[apply(node)]
    pub struct Case {
        case:      token::T!["case"],
        pub value: Box<Expr>,
        of:        token::T!["of"],
        pub arms:  Separated<case::Arm, token::K![";"]>,
        end:       token::T!["end"],
    }

    pub mod case {
        use super::*;

        #[apply(node)]
        pub struct Arm {
            pub pat:  Box<Pat>,
            arrow:    token::T!["->"],
            pub body: Box<Expr>,
        }
    }

    #[apply(node)]
    pub struct Annotated {
        l_paren:    token::T!["("],
        pub expr:   Box<Expr>,
        colon:      token::T![":"],
        pub r#type: Box<Type>,
        r_paren:    token::T![")"],
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

    pub type Wildcard = token::T!["_"];

    pub type Ident = token::T!["ident"];

    #[apply(node)]
    pub struct Variant {
        pub tag:     token::T!["#tag"],
        pub payload: Option<variant::Payload>,
    }

    pub mod variant {
        use super::*;

        #[apply(node)]
        pub struct Payload {
            l_paren:  token::T!["("],
            pub pats: Separated<Box<Pat>, token::K![","]>,
            r_paren:  token::T![")"],
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

    pub type Ident = token::T!["ident"];

    #[apply(node)]
    pub struct Enum {
        l_bracket:    token::T!["enum"],
        pub variants: Separated<r#enum::Variant, token::K![";"]>,
        r_bracket:    token::T!["end"],
    }

    pub mod r#enum {
        use super::*;

        #[apply(node)]
        pub struct Variant {
            pub tag:     token::T!["#tag"],
            pub payload: Option<variant::Payload>,
        }

        pub mod variant {
            use super::*;

            #[apply(node)]
            pub struct Payload {
                l_paren:   token::T!["("],
                pub types: Separated<Box<Type>, token::K![","]>,
                r_paren:   token::T![")"],
            }
        }
    }

    #[apply(node)]
    pub struct Function {
        function:        token::T!["function"],
        l_paren:         token::T!["("],
        pub params:      Separated<function::Param, token::K![","]>,
        r_paren:         token::T![")"],
        arrow:           token::T!["returns"],
        pub return_type: Box<Type>,
    }

    pub mod function {
        use super::*;

        #[apply(node)]
        pub struct Param {
            pub name:   token::T!["ident"],
            colon:      token::T![":"],
            pub r#type: Box<Type>,
        }
    }
}

#[apply(node)]
pub struct Module {
    module:             token::T!["module"],
    pub interface:      Option<module::Interface>,
    pub implementation: Option<module::Implementation>,
    end:                token::T!["end"],
}

pub mod module {
    use super::*;

    #[apply(node)]
    pub struct Interface {
        interface: token::T!["interface"],
        pub items: Vec<InterfaceItem>,
    }

    #[apply(node)]
    pub struct Implementation {
        implementation: token::T!["implementation"],
        pub items:      Vec<ImplementationItem>,
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
            r#type:    token::T!["type"],
            pub name:  token::T!["ident"],
            pub value: Option<type_alias::Value>,
            semicolon: token::T![";"],
        }

        pub mod type_alias {
            use super::*;

            #[apply(node)]
            pub struct Value {
                eq:         token::T!["="],
                pub r#type: Box<Type>,
            }
        }

        #[apply(node)]
        pub struct Function {
            function:        token::T!["function"],
            pub name:        token::T!["ident"],
            l_paren:         token::T!["("],
            pub params:      Separated<function::Param, token::K![","]>,
            r_paren:         token::T![")"],
            returns:         token::T!["returns"],
            return_name:     token::T!["ident"],
            return_colon:    token::T![":"],
            pub return_type: Box<Type>,
            semicolon:       token::T![";"],
        }

        pub mod function {
            use super::*;

            #[apply(node)]
            pub struct Param {
                pub name:   token::T!["ident"],
                colon:      token::T![":"],
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
            r#type:    token::T!["type"],
            pub name:  token::T!["ident"],
            eq:        token::T!["="],
            pub value: Box<Type>,
            semicolon: token::T![";"],
        }

        #[apply(node)]
        pub struct Function {
            function:        token::T!["function"],
            pub name:        token::T!["ident"],
            l_paren:         token::T!["("],
            pub params:      Separated<function::Param, token::K![","]>,
            r_paren:         token::T![")"],
            returns:         token::T!["returns"],
            pub return_name: token::T!["ident"],
            return_colon:    token::T![":"],
            pub return_type: Box<Type>,
            pub body:        expr::Block,
        }

        pub mod function {
            use super::*;

            #[apply(node)]
            pub struct Param {
                pub name:   token::T!["ident"],
                colon:      token::T![":"],
                pub r#type: Box<Type>,
            }
        }
    }
}

attribute_alias! {
    #[apply(node)] =
        #[apply(derive_parse)]
        #[token_set(token::Set)]
        #[apply(unpacked_debug)]
        #[derive(Clone, PartialEq, Eq)];
}
