extern crate proc_macro;

// #[macro_use] extern crate syn;
#[macro_use]
extern crate quote;
#[macro_use]
extern crate synstructure;

// use std::io::{self, Write};
use synstructure::{BindStyle, Structure};

decl_derive!([LocallyNameless] => locally_nameless_derive);

fn locally_nameless_derive(mut s: Structure) -> quote::Tokens {
    s.bind_with(|_| BindStyle::RefMut);

    let close_at_body = s.each(|bi| {
        quote!{
            ::nameless::LocallyNameless::close_at(#bi, index, name);
        }
    });

    let open_at_body = s.each(|bi| {
        quote!{
            ::nameless::LocallyNameless::open_at(#bi, index, name);
        }
    });

    s.bound_impl(
        quote!(::nameless::LocallyNameless),
        quote! {
            type Name = Name; // FIXME

            fn close_at(&mut self, index: ::nameless::Debruijn, name: &Self::Name) {
                match *self { #close_at_body }
            }

            fn open_at(&mut self, index: ::nameless::Debruijn, name: &Self::Name) {
                match *self { #open_at_body }
            }
        },
    )
}
