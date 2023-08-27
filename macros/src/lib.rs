use proc_macro2::{ Span };
use proc_macro::{ TokenStream, TokenTree };
use syn::{ LitStr, Ident };
use quote::quote;

#[proc_macro]
pub fn cap(input: TokenStream) -> TokenStream {
    // Initialize file variable (literal, identifier)
    let mut file = (None, None);

    // Initialize strings for direct, direct child, and any child permission tuples
    let mut direct_stream = None;
    let mut direct_child_stream = None;
    let mut any_child_stream = None;

    for node in input.into_iter() {
        match node {
            TokenTree::Literal(content) => {
                let raw = content.to_string();

                if raw == "with" {
                    continue;
                }

                if file.0.is_none() && file.1.is_none() {
                    file.0 = Some(LitStr::new(&raw[1..raw.len() - 1], Span::call_site()));
                    continue;
                }

                panic!("[Coenobita] [Error] Unexpected literal \"{}\"", raw);
            },

            TokenTree::Ident(content) => {
                let raw = content.to_string();

                if raw == "with" {
                    continue;
                }

                if file.0.is_none() && file.1.is_none() {
                    file.1 = Some(Ident::new(&raw,  Span::call_site()));
                    continue;
                }

                panic!("[Coenobita] [Error] Unexpected identifier \"{}\"", raw);
            },

            TokenTree::Group(content) => {
                if direct_stream.is_none() {
                    direct_stream = Some(content.stream());
                    continue;
                } else if direct_child_stream.is_none() {
                    direct_child_stream = Some(content.stream());
                    continue;
                } else {
                    any_child_stream = Some(content.stream());
                    continue;
                }
            },

            _ => continue
        }
    }

    // Must convert streams because quote! doesn't accept proc_macro::TokenStream
    let direct_stream = proc_macro2::TokenStream::from(direct_stream.unwrap());
    let direct_child_stream = proc_macro2::TokenStream::from(direct_child_stream.unwrap());
    let any_child_stream = proc_macro2::TokenStream::from(any_child_stream.unwrap());

    // Return with file path as string literal
    if file.0.is_some() {
        let final_file = file.0.unwrap();

        return quote! {
            coenobita::capability(#final_file, (#direct_stream), (#direct_child_stream), (#any_child_stream))
        }.into();
    }

    // Return with file path as identifier
    let final_file = file.1.unwrap();

    return quote! {
        coenobita::capability(#final_file, (#direct_stream), (#direct_child_stream), (#any_child_stream))
    }.into();
}
