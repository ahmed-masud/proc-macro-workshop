use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident};

fn is_option(ty: &syn::Type) -> bool {
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        if let Some(syn::PathSegment { ident, .. }) = path.segments.first() {
            return ident == "Option";
        }
    }
    false
}

fn inner_type(ty: &syn::Type) -> &syn::Type {
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        if let Some(syn::PathSegment { arguments, .. }) = path.segments.first() {
            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                args,
                ..
            }) = arguments
            {
                if let syn::GenericArgument::Type(ty) = args.first().unwrap() {
                    return ty;
                }
            }
        }
    }
    panic!("expected Option<...>");
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = syn::Ident::new(&format!("{}Builder", name), name.span());
    let fields = match input.data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => fields,
        _ => panic!("expected a struct"),
    };
    let builder_fields = fields.clone().into_iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        quote! {
            #name: ::std::option::Option<#ty>
        }
    });
    let builder_fields_default = fields.iter().map(|field| {
        let name = &field.ident;
        quote! {
            #name: None
        }
    });

    let builder_set_methods = fields.iter().map(|field| {
        let name = &field.ident;
        let attrs = &field.attrs;

        let (ty, option_stripped) = match &field.ty {
            ty if is_option(ty) => (inner_type(ty), true),
            ty => (ty, false),
        };
        match (attrs.len(), option_stripped) {
            (0, false) => {
                return quote! {
                    pub fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            }
            (0, true) => {
                return quote! {
                    pub fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(Some(#name));
                        self
                    }
                }
            }
            _ => (),
        }


       let expanded = attrs.iter().map(|attr| {
            let attr = attr.parse_meta().unwrap();
            if !attr.path().is_ident("builder") {
                return quote!();
            }
            let mut nested = match attr {
                syn::Meta::List(syn::MetaList { nested, .. }) => nested,
                _ => return quote!(),
            };
            if nested.len() != 1 {
                panic!("expected exactly argument to builder each = \"name\"");
            }
            let single_name = match nested.pop().unwrap().into_value() {
                syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                    lit: syn::Lit::Str(lit_str), ..
                })) => {
                    let Ok(arg) = lit_str.parse::<Ident>()
                    else {
                        panic!("{} is not a valid identifier", lit_str.value());
                    };
                    arg
                },
                _ => panic!("expected exactly argument to builder each = \"name\""),
            };
            let inner_type = inner_type(ty);
            return quote! {
                pub fn #single_name(&mut self, value: #inner_type) -> &mut Self {
                    let v: &mut #ty = self.#name.get_or_insert_with(||vec![]);
                    v.push(value);
                    self
                }
            }
        });

        quote! {
            #( #expanded )*
        }
    });

    let build_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        let is_builder = field.attrs.iter().any(|attr| attr.parse_meta().unwrap().path().is_ident("builder"));
        if is_option(&ty) {
            return quote! {
                #name: if self.#name.is_some() {
                    self.#name.clone().unwrap()
                } else {
                    None
                }
            };
        }
        if is_builder {
            return quote! {
                #name: if self.#name.is_some() {
                    self.#name.clone().unwrap()
                } else {
                    vec![]
                }
            };
        }
        quote! {
            #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is required"))?
        }
    });

    let tokens = quote! {
        pub struct #builder_name {
            #(#builder_fields,)*
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name::new()
            }
        }

        impl #builder_name {
            pub fn new() -> Self {
                Self {
                    #(#builder_fields_default,)*
                }
            }

            #(#builder_set_methods)*

            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_fields,)*
                })
            }
        }
    };
    let r = TokenStream::from(tokens);
    r
}
