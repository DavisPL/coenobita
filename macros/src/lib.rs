use proc_macro2::{ Group, Literal, Ident };
use proc_macro::{ TokenStream, TokenTree };
use syn::{ parse_str };
use quote::quote;

#[derive(PartialEq)]
enum PermissionType {
    DirectObject,
    DirectChild,
    AnyChild,
    None
}

fn permission_group_string(list: [&str; 8]) -> String {
    let mut output = String::from("(");

    for perm in list.iter() {
        output = output + perm + ",";
    }

    output + ")"
}

#[proc_macro]
pub fn cap(input: TokenStream) -> TokenStream {
    let mut file = String::from("");
    let mut file_is_literal = true;

    let mut pending_perms = PermissionType::DirectObject;
    
    // Create, View, Read, Write, Append, Copy, Move, Delete
    let mut direct_object = ["()", "()", "()", "()", "()", "()", "()", "()"];
    let mut direct_child = ["()", "()", "()", "()", "()", "()", "()", "()"];
    let mut any_child = ["()", "()", "()", "()", "()", "()", "()", "()"];

    for node in input.into_iter() {
        match node {
            TokenTree::Literal(content) => {
                let raw = content.to_string();

                if raw == "with" {
                    continue;
                }

                if file == "" {
                    file = String::from(&raw[1..raw.len() - 1]);
                    continue;
                }

                panic!("[Coenobita] [Error] Unexpected literal \"{}\"", raw);
            },

            TokenTree::Ident(content) => {
                let raw = content.to_string();

                if raw == "with" {
                    continue;
                }

                if file == "" {
                    file = String::from(raw);
                    file_is_literal = false;
                    continue;
                }

                panic!("[Coenobita] [Error] Unexpected identifier \"{}\"", raw);
            },
            
            TokenTree::Group(content) => {
                for subnode in content.stream().into_iter() {
                    match subnode {
                        TokenTree::Ident(content) => {
                            let raw = content.to_string();

                            match pending_perms {
                                PermissionType::DirectObject => {
                                    match raw.as_ref() {
                                        "Create" => direct_object[0] = "coenobita::Create",
                                        "View"   => direct_object[1] = "coenobita::View",
                                        "Read"   => direct_object[2] = "coenobita::Read",
                                        "Write"  => direct_object[3] = "coenobita::Write",
                                        "Append" => direct_object[4] = "coenobita::Append",
                                        "Copy"   => direct_object[5] = "coenobita::Copy",
                                        "Move"   => direct_object[6] = "coenobita::Move",
                                        "Delete" => direct_object[7] = "coenobita::Delete",
                                               _ => {
                                            panic!("[Coenobita] [Error] Unexpected permission identifier \"{}\" != \"{}\" != \"{}\"", content.to_string(), raw, "Delete")
                                        }
                                    }
                                },

                                PermissionType::DirectChild => {
                                    match raw.as_ref() {
                                        "Create" => direct_child[0] = "coenobita::Create",
                                        "View"   => direct_child[1] = "coenobita::View",
                                        "Read"   => direct_child[2] = "coenobita::Read",
                                        "Write"  => direct_child[3] = "coenobita::Write",
                                        "Append" => direct_child[4] = "coenobita::Append",
                                        "Copy"   => direct_child[5] = "coenobita::Copy",
                                        "Move"   => direct_child[6] = "coenobita::Move",
                                        "Delete" => direct_child[7] = "coenobita::Delete",
                                               _ => panic!("[Coenobita] [Error] Unexpected permission identifier \"{}\" 2", raw)
                                    }
                                },

                                PermissionType::AnyChild => {
                                    match raw.as_ref() {
                                        "Create" => any_child[0] = "coenobita::Create",
                                        "View"   => any_child[1] = "coenobita::View",
                                        "Read"   => any_child[2] = "coenobita::Read",
                                        "Write"  => any_child[3] = "coenobita::Write",
                                        "Append" => any_child[4] = "coenobita::Append",
                                        "Copy"   => any_child[5] = "coenobita::Copy",
                                        "Move"   => any_child[6] = "coenobita::Move",
                                        "Delete" => any_child[7] = "coenobita::Delete",
                                               _ => panic!("[Coenobita] [Error] Unexpected permission identifier \"{}\" 3", raw)
                                    }
                                },

                                PermissionType::None => panic!("[Coenobita] [Error] Unexpected group after permission tuple for any child")
                            }

                        },

                        _ => panic!("[Coenobita] [Error] Unexpected token in permission tuple")
                    }
                }

                match pending_perms {
                    PermissionType::DirectObject => pending_perms = PermissionType::DirectChild,
                    PermissionType::DirectChild  => pending_perms = PermissionType::AnyChild,
                    PermissionType::AnyChild     => pending_perms = PermissionType::None,
                                                _ => break
                }
            },

            _ => continue
        }
    }

    let direct_object_string: Group = parse_str(&permission_group_string(direct_object)).unwrap();
    let direct_child_string: Group = parse_str(&permission_group_string(direct_child)).unwrap();
    let any_child_string: Group = parse_str(&permission_group_string(any_child)).unwrap();

    // The file should be a string literal
    if file_is_literal {
        let file_literal: Literal = parse_str(&file).unwrap();
    
        return quote! {
            coenobita::capability(#file_literal, #direct_object_string, #direct_child_string, #any_child_string)
        }.into();
    }

    // The file should be an identifier
    let file_ident: Ident = parse_str(&file).unwrap();

    quote! {
        coenobita::capability(#file_ident, #direct_object_string, #direct_child_string, #any_child_string)
    }.into()
}

