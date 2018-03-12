extern crate proc_macro;

#[macro_use]
extern crate quote;
extern crate syn;
#[macro_use]
extern crate synstructure;

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

decl_derive!([AlphaEq] => alpha_eq_derive);

fn alpha_eq_derive(mut s: Structure) -> quote::Tokens {
    s.bind_with(|_| BindStyle::Ref);

    let alpha_eq_body = {
        let body = s.variants().iter().fold(quote!(), |acc, v| {
            // Create two sets of bindings, one for the lhs, and another for the rhs
            let mut lhs = v.clone();
            let mut rhs = v.clone();
            lhs.binding_name(|_, i| syn::Ident::from(format!("__binding_lhs_{}", i)));
            rhs.binding_name(|_, i| syn::Ident::from(format!("__binding_rhs_{}", i)));

            let lhs_pat = lhs.pat();
            let rhs_pat = rhs.pat();

            // build up the alpha-equality expression for this variant
            let arm_body = <_>::zip(lhs.bindings().iter(), rhs.bindings()).fold(
                quote!(true),
                |acc, (lhs, rhs)| {
                    quote! { #acc && ::nameless::AlphaEq::alpha_eq(#lhs, #rhs) }
                },
            );

            quote! { #acc (&#lhs_pat, &#rhs_pat) => #arm_body, }
        });

        // Avoid the 'unreachable match' warning for types with zero or one variants
        match s.variants().len() {
            0 | 1 => body,
            _ => quote! { #body (_, _) => false },
        }
    };

    s.bound_impl(
        quote!(::nameless::AlphaEq),
        quote! {
            fn alpha_eq(&self, other: &Self) -> bool {
                match (self, other) { #alpha_eq_body }
            }
        },
    )
}
