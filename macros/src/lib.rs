use proc_macro2::{ Span };
use proc_macro::{ TokenStream, TokenTree };
use syn::{ LitStr, TypeParam, Ident, Path, Type };
use quote::quote;

fn token_code_to_string(code: i32) -> &'static str {
    match code {
        0 => "literal",
        1 => "punctuation",
        2 => "identifier",
        _ => "unknown"
    }
}

#[proc_macro]
pub fn cap(input: TokenStream) -> TokenStream {
    let mut file_path = LitStr::new("", Span::call_site());
    let mut read_type = TypeParam::from(Ident::new("NotGranted", Span::call_site()));
    let mut write_type = TypeParam::from(Ident::new("NotGranted", Span::call_site()));
    let mut copy_type = TypeParam::from(Ident::new("NotGranted", Span::call_site()));
    let mut move_type = TypeParam::from(Ident::new("NotGranted", Span::call_site()));
    let mut delete_type = TypeParam::from(Ident::new("NotGranted", Span::call_site()));

    let mut rtypepath: syn::TypePath;

    let mut expected_token = 0; // 0 = Literal | 1 = Punct | 2 = Ident
    let mut expected_value = "";

    for node in input.into_iter() {
        match node {
            TokenTree::Literal(content) => {
                if expected_token != 0 {
                    panic!(
                        "[Coenobita] ERROR - Expected {}, found literal with value {}",
                        token_code_to_string(expected_token),
                        content.to_string()
                    );
                }

                let undoctored_filepath: &str = &content.to_string();

                file_path = LitStr::new(
                    &(undoctored_filepath)[1..undoctored_filepath.len() - 1], 
                    Span::call_site()
                );

                // Now we expect identifier with value "with"
                expected_token = 2; 
                expected_value = "with";
            },

            TokenTree::Punct(content) => {
                if expected_token != 1 {
                    panic!(
                        "[Coenobita] ERROR - Expected {}, found punctuation with value '{}'",
                        token_code_to_string(expected_token),
                        content.as_char()
                    );
                }

                if content.as_char() == ',' {
                    // Punctuation is valid, expect permission identifier
                    expected_token = 2;
                    expected_value = "";
                } else {
                    panic!(
                        "[Coenobita] ERROR - Incorrect punctuation. Expected ',', found '{}'",
                        content.as_char()
                    );
                }
            },

            TokenTree::Ident(content) => {
                let identifier = content.to_string();
                
                // Make sure we were expecting an identifier
                if expected_token != 2 {
                    panic!(
                        "[Coenobita] ERROR - Expected {}, found identifier with value {}",
                        token_code_to_string(expected_token),
                        identifier
                    );
                }
                
                // Check whether the identifier is "with"
                if identifier == "with" && expected_value == "with" {
                    // Now expecting an identifier representing a permission
                    expected_token = 2;
                    expected_value = "";
                } else {
                    match identifier.as_ref() {
                        "Read"   => read_type   = TypeParam::from(Ident::new("Read", Span::call_site())),
                        "Write"  => write_type  = TypeParam::from(Ident::new("Write", Span::call_site())),
                        "Copy"   => copy_type   = TypeParam::from(Ident::new("Copy", Span::call_site())),
                        "Move"   => move_type   = TypeParam::from(Ident::new("Move", Span::call_site())),
                        "Delete" => delete_type = TypeParam::from(Ident::new("Delete", Span::call_site())),
                               _ => panic!("[Coenobita] ERROR - Unexpected permission \"{}\"", identifier)
                        /*"Read" => {
                            let path_str = "coenobita::Read"; // The fully qualified path as a string
                            let path: Path = syn::parse_str(path_str).unwrap();
                            let type_path = syn::TypePath {
                                qself: None,
                                path,
                            };
                        },

                        _ => println!("DEBUG - Do nothing")*/
                    }

                    // Now expecting punctuation token with value ','
                    expected_token = 1;
                    expected_value = ",";
                }
            }

            _ => {
                panic!("[Coenobita] ERROR - Unexpected token type in capability creation");
            }
        }
    }

    quote! {{
        let capability: Capability<coenobita::#read_type, coenobita::#write_type, coenobita::#copy_type, coenobita::#move_type, coenobita::#delete_type> = Capability::new(#file_path);
        capability
    }}.into()
}
