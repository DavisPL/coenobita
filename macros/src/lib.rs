use proc_macro::TokenStream;
use proc_macro2::Group;
use quote::quote;
use syn::{parse_macro_input, Expr, Path, Token, token, parse_str};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::parenthesized;
use syn::Ident;

fn permission_group_string(list: [&str; 8]) -> String {
    let mut output = String::from("(");

    for perm in list.iter() {
        output = output + perm + ",";
    }

    output + ")"
}

// Define a custom struct to hold the path and permissions
struct PathAndPermissions<'a> {
    path: Expr,
    permissions: [[&'a str; 8]; 3],
}

// Implement the Parse trait for your custom struct
impl<'a> Parse for PathAndPermissions<'a> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse the path as a string literal or expression
        let path: Expr = input.parse()?;

        // Parse the optional "with" identifier and ignore it
        let _with: Option<Ident> = input.parse().ok();

        // Create, View, Read, Write, Append, Copy, Move, Delete
        let mut permission_index = 0;
        let mut permissions = [
            ["()", "()", "()", "()", "()", "()", "()", "()"],
            ["()", "()", "()", "()", "()", "()", "()", "()"],
            ["()", "()", "()", "()", "()", "()", "()", "()"]
        ];

        while permission_index < 3 {
            if input.cursor().eof() {
                if permission_index == 0 {
                    panic!("[Coenobita] [Error] At least one permission set required.");
                }

                break;
            }

            if input.peek(token::Comma) {
                let _ = input.parse::<Comma>();
                continue;
            }

            if !input.peek(token::Paren) {
                panic!("[Coenobita] [Error] Expected parenthesis, found another token.");
            }

            let content;
            parenthesized!(content in input);
            let permission_list: Punctuated<Path, Comma> = content.parse_terminated(Path::parse, Token!(,))?;
            
            for item in permission_list.iter() {
                let perm_string = item.segments[0].ident.to_string();

                match perm_string.as_str() {
                    "Create" => permissions[permission_index][0] = "coenobita::Create",
                    "View"   => permissions[permission_index][1] = "coenobita::View",
                    "Read"   => permissions[permission_index][2] = "coenobita::Read",
                    "Write"  => permissions[permission_index][3] = "coenobita::Write",
                    "Append" => permissions[permission_index][4] = "coenobita::Append",
                    "Copy"   => permissions[permission_index][5] = "coenobita::Copy",
                    "Move"   => permissions[permission_index][6] = "coenobita::Move",
                    "Delete" => permissions[permission_index][7] = "coenobita::Delete",
                            _ => {
                        panic!("[Coenobita] [Error] Unexpected permission identifier \"{}\" != \"{}\" != \"{}\"", content.to_string(), perm_string, "Delete")
                    }
                }
            }

            permission_index += 1;
        }

        Ok(PathAndPermissions { path, permissions })
    }
}

// Define your macro
#[proc_macro]
pub fn cap(input: TokenStream) -> TokenStream {
    // Parse the input using the custom struct
    let path_and_permissions: PathAndPermissions = parse_macro_input!(input);

    // Extract the path and permissions
    let path = path_and_permissions.path;
    let permissions = path_and_permissions.permissions;

    let direct: Group = parse_str(&permission_group_string(permissions[0])).unwrap();
    let imm_child: Group = parse_str(&permission_group_string(permissions[1])).unwrap();
    let any_child: Group = parse_str(&permission_group_string(permissions[2])).unwrap();

    // Generate the output struct using the extracted values
    quote! {
        coenobita::capability(#path, #direct, #imm_child, #any_child)
    }.into()
}

