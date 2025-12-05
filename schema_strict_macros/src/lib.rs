use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{
    Attribute, Expr, Fields, Ident, ItemEnum, Lit, LitStr, Meta, MetaNameValue, Token,
    parse_macro_input, punctuated::Punctuated, spanned::Spanned,
};

/// Attribute macro for string enums that need compact `type/enum` schemas compatible with OpenAI strict mode.
/// Usage:
/// ```rust
/// #[derive(Clone, Debug)]
/// #[schema_strict::string_enum(description = "Fulfillment modes")]
/// #[serde(rename_all = "snake_case")]
/// enum FulfillmentMode { Delivery, Pickup }
/// ```
#[proc_macro_attribute]
pub fn string_enum(args: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args with Punctuated::<Meta, Token![,]>::parse_terminated);
    let mut description: Option<LitStr> = None;
    let mut nullable = false;

    for arg in args {
        match arg {
            Meta::NameValue(MetaNameValue { path, value, .. }) if path.is_ident("description") => {
                if let Some(lit) = lit_str_from_expr(&value) {
                    description = Some(lit);
                } else {
                    return compile_error("description must be a string literal", value.span());
                }
            }
            Meta::Path(path) if path.is_ident("nullable") => {
                nullable = true;
            }
            Meta::NameValue(MetaNameValue { path, value, .. }) if path.is_ident("nullable") => {
                if let Some(lit) = lit_bool_from_expr(&value) {
                    nullable = lit;
                } else {
                    return compile_error("nullable must be a boolean literal", value.span());
                }
            }
            other => {
                return compile_error(
                    "string_enum supports: description = \"...\", nullable or nullable = true/false",
                    other.span(),
                );
            }
        }
    }

    let input = parse_macro_input!(item as ItemEnum);

    // Ensure all variants are unit variants.
    for variant in &input.variants {
        if !matches!(variant.fields, Fields::Unit) {
            return compile_error(
                "string_enum only supports unit variants (no payloads).",
                variant.span(),
            );
        }
    }

    // Capture serde rename_all on the enum, if present.
    let rename_all_rule = find_serde_rename_all(&input.attrs);

    // Compute serialized names for each variant (respecting serde rename / rename_all).
    let mut variant_names = Vec::with_capacity(input.variants.len());
    for variant in &input.variants {
        if let Some(rename) = find_serde_rename(&variant.attrs) {
            variant_names.push(rename);
        } else if let Some(rule) = rename_all_rule.as_deref() {
            match apply_rename_all(&variant.ident, rule) {
                Ok(name) => variant_names.push(name),
                Err(ts) => return ts,
            }
        } else {
            variant_names.push(variant.ident.to_string());
        }
    }

    let variant_lits: Vec<LitStr> = variant_names
        .iter()
        .map(|name| LitStr::new(name, Span::call_site()))
        .collect();

    let enum_ident = input.ident.clone();
    let desc_tokens = match description {
        Some(d) => quote!(Some(#d)),
        None => quote!(None),
    };
    let builder_fn = if nullable {
        quote!(::schema_strict::string_enum_nullable_schema)
    } else {
        quote!(::schema_strict::string_enum_schema)
    };

    let expanded = quote! {
        #input

        impl schemars::JsonSchema for #enum_ident {
            fn inline_schema() -> bool {
                true
            }

            fn schema_name() -> std::borrow::Cow<'static, str> {
                std::borrow::Cow::Owned(stringify!(#enum_ident).to_string())
            }

            fn json_schema(_: &mut schemars::SchemaGenerator) -> schemars::Schema {
                const VARIANTS: &[&str] = &[#(#variant_lits),*];
                #builder_fn(VARIANTS, #desc_tokens)
            }
        }
    };

    expanded.into()
}

fn find_serde_rename(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if !attr.path().is_ident("serde") {
            continue;
        }
        let mut rename: Option<String> = None;
        let _ = attr.parse_nested_meta(|nested| {
            if nested.path.is_ident("rename") {
                if let Ok(lit) = nested.value()?.parse::<LitStr>() {
                    rename = Some(lit.value());
                }
            }
            Ok(())
        });
        if rename.is_some() {
            return rename;
        }
    }
    None
}

fn find_serde_rename_all(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if !attr.path().is_ident("serde") {
            continue;
        }
        let mut rename_all: Option<String> = None;
        let _ = attr.parse_nested_meta(|nested| {
            if nested.path.is_ident("rename_all") {
                if let Ok(lit) = nested.value()?.parse::<LitStr>() {
                    rename_all = Some(lit.value());
                }
            }
            Ok(())
        });
        if rename_all.is_some() {
            return rename_all;
        }
    }
    None
}

fn apply_rename_all(ident: &Ident, rule: &str) -> Result<String, TokenStream> {
    let raw = ident.to_string();
    let renamed = match rule {
        "lowercase" => raw.to_ascii_lowercase(),
        "UPPERCASE" => raw.to_ascii_uppercase(),
        "snake_case" => raw.to_case(Case::Snake),
        "SCREAMING_SNAKE_CASE" => raw.to_case(Case::UpperSnake),
        "kebab-case" => raw.to_case(Case::Kebab),
        "SCREAMING-KEBAB-CASE" => raw.to_case(Case::UpperKebab),
        "camelCase" => raw.to_case(Case::Camel),
        "PascalCase" => raw.to_case(Case::Pascal),
        other => {
            return Err(compile_error(
                &format!("unsupported serde rename_all value: {other}"),
                ident.span(),
            ));
        }
    };
    Ok(renamed)
}

fn compile_error(message: &str, span: Span) -> TokenStream {
    let lit = LitStr::new(message, span);
    quote!( compile_error!(#lit); ).into()
}

fn lit_str_from_expr(expr: &Expr) -> Option<LitStr> {
    if let Expr::Lit(syn::ExprLit {
        lit: Lit::Str(s), ..
    }) = expr
    {
        Some(s.clone())
    } else {
        None
    }
}

fn lit_bool_from_expr(expr: &Expr) -> Option<bool> {
    if let Expr::Lit(syn::ExprLit {
        lit: Lit::Bool(b), ..
    }) = expr
    {
        Some(b.value)
    } else {
        None
    }
}
