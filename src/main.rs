#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(debug_closure_helpers)]
#![feature(assert_matches)]
#![feature(const_trait_impl)]
#![feature(maybe_uninit_array_assume_init)]
#![feature(slice_as_array)]
#![feature(try_blocks)]

pub mod parser;
pub mod separated;
pub mod token;

pub mod example;

fn main() {
    println!("Hello, world!");
}
