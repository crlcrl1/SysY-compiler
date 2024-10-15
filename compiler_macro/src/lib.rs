use proc_macro::TokenStream;
use quote::quote;
use syn::{Attribute, Data, DeriveInput, Expr, Fields, Lit, Meta};

#[proc_macro_derive(Inst, attributes(asm_name))]
pub fn inst_macro(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    inst_macro_impl(ast)
}

fn get_asm_name(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if attr.path().is_ident("asm_name") {
            if let Meta::NameValue(ref meta) = attr.meta {
                return match meta.value {
                    Expr::Lit(ref lit) => match lit.lit {
                        Lit::Str(ref s) => Some(s.value()),
                        _ => None,
                    },
                    _ => None,
                };
            }
        }
    }
    None
}

fn inst_macro_impl(ast: DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let asm_name = get_asm_name(&ast.attrs).unwrap_or_else(|| name.to_string().to_lowercase());
    let fields = match ast.data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => fields.named.iter().collect::<Vec<_>>(),
            Fields::Unit => vec![],
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };
    let fields = fields
        .iter()
        .map(|field| {
            let ident = field.ident.as_ref().unwrap();
            ident
        })
        .collect::<Vec<_>>();
    let mut format_str = asm_name;
    fields.iter().for_each(|field| {
        if field.to_string() == "offset" {
            format_str.push_str(" ({})");
        } else {
            format_str.push_str(" {}");
        }
    });
    let gen = quote! {
        impl Inst for #name {
            fn dump(&self) -> String {
                format!(#format_str, #(self.#fields.to_string(),)*)
            }
        }
    };
    gen.into()
}
