use proc_macro2::{ Span };
use proc_macro::{ TokenStream, TokenTree };
use syn::{ LitStr, Type, parse_str };
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
pub fn dir(input: TokenStream) -> TokenStream {
    let mut sub_cap = syn::Ident::new("CAPABILITY", Span::call_site());
    let mut read_type: Type = parse_str("()").unwrap();
    let mut write_type: Type = parse_str("()").unwrap();
    let mut copy_type: Type = parse_str("()").unwrap();
    let mut move_type: Type = parse_str("()").unwrap();
    let mut delete_type: Type = parse_str("()").unwrap();

    let mut expected_token = 2; // 0 = Literal | 1 = Punct | 2 = Ident
    let mut expected_value = "";

    for node in input.into_iter() {
        match node {
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
                    // Now expecting punctuation token with value ','
                    expected_token = 1;
                    expected_value = ",";
                    
                    match identifier.as_ref() {
                        "Read"   => read_type   = parse_str("coenobita::Read").unwrap(),
                        "Write"  => write_type  = parse_str("coenobita::Write").unwrap(),
                        "Copy"   => copy_type   = parse_str("coenobita::Copy").unwrap(),
                        "Move"   => move_type   = parse_str("coenobita::Move").unwrap(),
                        "Delete" => delete_type = parse_str("coenobita::Delete").unwrap(),
                               _ => {
                            sub_cap = syn::Ident::new(identifier.as_ref(), Span::call_site());
                            expected_token = 2;
                            expected_value = "with";
                        }
                    }
                }
            }

            _ => {
                panic!("[Coenobita] ERROR - Unexpected token type in capability creation");
            }
        }
    }

    quote! {{
        let directory = Directory::<Capability<#read_type, #write_type, #copy_type, #move_type, #delete_type>, ()>::from(#sub_cap);
        directory
    }}.into()
}

#[proc_macro]
pub fn cap(input: TokenStream) -> TokenStream {
    let mut using_literal = true;
    let mut file_path = LitStr::new("", Span::call_site());
    let mut file_identifier = syn::Ident::new("FILE", Span::call_site());

    let mut read_type: Type = parse_str("()").unwrap();
    let mut write_type: Type = parse_str("()").unwrap();
    let mut copy_type: Type = parse_str("()").unwrap();
    let mut move_type: Type = parse_str("()").unwrap();
    let mut delete_type: Type = parse_str("()").unwrap();

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
                if expected_token == 0 {
                    // User must be using an identifier for a path or string instead of a literal
                    let identifier = content.to_string();
                    file_identifier = syn::Ident::new(&&&&&identifier, Span::call_site());
                    using_literal = false;

                    // Now we expect identifier with value "with"
                    expected_token = 2; 
                    expected_value = "with";

                    continue;
                }

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
                        "Read"   => read_type   = parse_str("coenobita::Read").unwrap(),
                        "Write"  => write_type  = parse_str("coenobita::Write").unwrap(),
                        "Copy"   => copy_type   = parse_str("coenobita::Copy").unwrap(),
                        "Move"   => move_type   = parse_str("coenobita::Move").unwrap(),
                        "Delete" => delete_type = parse_str("coenobita::Delete").unwrap(),
                               _ => panic!("[Coenobita] ERROR - Unexpected permission \"{}\"", identifier)
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

    if using_literal {
        return quote! {{
            let capability: Capability<#read_type, #write_type, #copy_type, #move_type, #delete_type> = Capability::new(#file_path);
            capability
        }}.into();
    }

    quote! {{
        let capability: Capability<#read_type, #write_type, #copy_type, #move_type, #delete_type> = Capability::new(#file_identifier);
        capability
    }}.into()
}
