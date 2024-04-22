#![feature(proc_macro_quote)]
extern crate proc_macro;

mod parse;

use parse::{Attribute, Attributes, Field, ParseExt, Parser, TokenExt, Type, Variant};
use proc_macro::{quote, Delimiter, Group, Literal, TokenStream, TokenTree};

use crate::parse::{Generic, VariantKind};

#[proc_macro_derive(Serialize, attributes(jsonette))]
pub fn derive_serialize(item: TokenStream) -> TokenStream {
    let mut tokens = item.into_iter().peekable();
    let attributes = tokens.try_attributes();
    tokens.eat_visibility();

    match &*tokens.try_ident().unwrap().to_string() {
        "struct" => derive_for_struct(tokens, attributes),
        "enum" => derive_for_enum(tokens, attributes),
        _ => unreachable!(),
    }
}

#[proc_macro_derive(Deserialize, attributes(jsonette))]
pub fn derive_deserialize_entry(item: TokenStream) -> TokenStream {
    let mut tokens = item.into_iter().peekable();
    let attributes = tokens.try_attributes();
    derive_deserialize(tokens, &attributes)
}

fn derive_deserialize(mut tokens: Parser, attributes: &Attributes) -> TokenStream {
    tokens.eat_visibility();

    match &*tokens.try_ident().unwrap().to_string() {
        "struct" => {}
        "enum" => return deserialize_enum(tokens, attributes),
        _ => unreachable!(),
    }

    let struct_name = tokens.try_type().unwrap();
    // panic!("{:?}", tokens.collect::<TokenStream>());
    let group = tokens.try_group().unwrap();

    match group.delimiter() {
        Delimiter::Brace => generate_deserialize_for_named_struct(struct_name, group, attributes),
        Delimiter::Parenthesis => {
            generate_deserialize_for_tuple_struct(struct_name, group, attributes)
        }
        Delimiter::None => panic!("empty structs unsupported"),
        Delimiter::Bracket => unreachable!(),
    }
}

fn deserialize_enum(mut tokens: Parser, attributes: &Attributes) -> TokenStream {
    if attributes.contains(&Attribute::Repr) {
        return deserialize_repr_enum(tokens, attributes);
    }

    if attributes.contains(&Attribute::Untagged) {
        return deserialize_untagged_enum(tokens, attributes);
    }

    let ty = tokens.try_type().unwrap();
    let variants = tokens.try_group().unwrap().parser().try_enum_variants();
    let additional_generics = ty.additional_generics();
    let where_clause = ty.where_clause_deserialize();

    let primitive_variants: Vec<&Variant> = variants
        .iter()
        .filter(|v| matches!(v.kind, parse::VariantKind::Unit))
        .collect();
    let complex_variants: Vec<&Variant> = variants
        .iter()
        .filter(|v| !matches!(v.kind, parse::VariantKind::Unit))
        .collect();

    let primitive_variant_matches: TokenStream = primitive_variants
        .iter()
        .map(|variant| {
            let tag = variant.tag.stream();
            let tag_literal = variant.tag_literal(&attributes);
            let parse_variant = match variant.kind {
                parse::VariantKind::Unit => quote!(Some(Self::$tag),),
                _ => unreachable!(),
            };

            quote! {
                $tag_literal => $parse_variant
            }
        })
        .collect();

    let all_generics = ty.generics();

    let variant_matches: TokenStream = complex_variants
        .iter()
        .map(|variant| {
            let tag = variant.tag.stream();
            let tag_literal = variant.tag_literal(&attributes);
            // let name = variant.tokens();
            let parse_variant = match variant.kind {
                parse::VariantKind::Tuple => match variant.fields.len() {
                    1 => {
                        let field = &variant.fields[0];
                        let ty = field.ty.tokens_turbofish();
                        quote!(Some(Self::$tag(parser.parse_deserialize::<$ty>()?)))
                    }
                    _ => {
                        let parsing: TokenStream = variant
                            .fields
                            .iter()
                            .enumerate()
                            .map(|(i, field)| {
                                let name: TokenStream = format!("__field_{}", i).parse().unwrap();
                                let ty = field.ty.tokens_turbofish();
                                quote!(let $name = parser.parse_deserialize::<$ty>()?;)
                            })
                            .collect();

                        let field_names: TokenStream = (0..variant.fields.len())
                            .map(|i| {
                                let name: TokenStream = format!("__field_{}", i).parse().unwrap();
                                quote!($name,)
                            })
                            .collect();

                        quote! {
                            parser.parse_array_start()?;
                            $parsing
                            parser.parse_array_end()?;
                            Some(Self::$tag($field_names))
                        }
                    }
                },
                parse::VariantKind::Struct => {
                    let delegate_name: TokenStream = format!("__Delegate_{}", tag).parse().unwrap();

                    let fields: TokenStream = variant
                        .fields
                        .iter()
                        .map(|field| {
                            let name = field.name.as_ref().unwrap().stream();
                            quote!($name,)
                        })
                        .collect();

                    let field_definitions: TokenStream = variant
                        .fields
                        .iter()
                        .map(|field| {
                            let name = field.name.as_ref().unwrap().stream();
                            let ty = field.ty.tokens();
                            quote!($name: $ty,)
                        })
                        .collect();

                    let generics: Vec<Generic> = variant
                        .fields
                        .iter()
                        .filter_map(|f| {
                            all_generics
                                .iter()
                                .cloned()
                                .find(|ty| ty.tokens().to_string() == f.ty.ident().to_string())
                        })
                        .collect();

                    let delegate_generics: TokenStream = generics
                        .iter()
                        .map(|g| g.tokens())
                        .map(|g| quote!($g,))
                        .collect();

                    let delegate = quote! {
                        pub struct $delegate_name<$delegate_generics> {
                            $field_definitions
                        }
                    };

                    let delegate_deserialize_impl =
                        derive_deserialize(delegate.clone().into_iter().peekable(), &attributes);

                    // TODO: forward generics to delegate
                    quote! {
                        $delegate
                        $delegate_deserialize_impl

                        let $delegate_name::<$delegate_generics> {
                            $fields
                        } = parser.parse_deserialize::<$delegate_name<$delegate_generics>>()?;
                        Some(Self::$tag {
                            $fields
                        })
                    }
                }
                parse::VariantKind::Unit => unreachable!(),
            };

            quote! {
                $tag_literal => { $parse_variant }
            }
        })
        .collect();

    quote! {
        impl<'de, $additional_generics> ::jsonette::Deserialize<'de> for $ty $where_clause {
            fn deserialize<R: ::jsonette::Read<'de>>(parser: &mut ::jsonette::Parser<R>) -> Option<Self> {
                match parser.peek()? {
                    b'{' => {
                        let _ = parser.next();
                        parser.advance();
                        match parser.parse_name()? {
                            $variant_matches
                            s => None,
                        }
                    }
                    b'"' => {
                        match parser.parse_deserialize::<&str>()? {
                            $primitive_variant_matches
                            s => None,
                        }
                    }
                    _ => None,
                }
            }
        }
    }
}

fn deserialize_untagged_enum(mut tokens: Parser, attributes: &Attributes) -> TokenStream {
    let ty = tokens.try_type().unwrap();
    let variants = tokens.try_group().unwrap().parser().try_enum_variants();
    let additional_generics = ty.additional_generics();
    let where_clause = ty.where_clause_deserialize();
    let all_generics = ty.generics();

    let struct_variants: Vec<&Variant> = variants
        .iter()
        .filter(|v| matches!(v.kind, parse::VariantKind::Struct))
        .collect();
    let tuple_variants: Vec<&Variant> = variants
        .iter()
        .filter(|v| matches!(v.kind, parse::VariantKind::Tuple))
        .filter(|v| v.fields.len() > 1)
        .collect();
    let newtype_variants: Vec<&Variant> = variants
        .iter()
        .filter(|v| matches!(v.kind, parse::VariantKind::Tuple))
        .filter(|v| v.fields.len() == 1)
        .collect();
    let primitive_variants: Vec<&Variant> = variants
        .iter()
        .filter(|v| matches!(v.kind, parse::VariantKind::Unit))
        .collect();

    let try_structs: TokenStream = struct_variants
        .iter()
        .map(|variant| {
            let tag = variant.tag.stream();
            let delegate_name: TokenStream = format!("__Delegate_{}", tag).parse().unwrap();

            let fields: TokenStream = variant
                .fields
                .iter()
                .map(|field| {
                    let name = field.name.as_ref().unwrap().stream();
                    quote!($name,)
                })
                .collect();

            let field_definitions: TokenStream = variant
                .fields
                .iter()
                .map(|field| {
                    let name = field.name.as_ref().unwrap().stream();
                    let ty = field.ty.tokens();
                    quote!($name: $ty,)
                })
                .collect();

            let generics: Vec<Generic> = variant
                .fields
                .iter()
                .filter_map(|f| {
                    all_generics.iter().cloned()
                        .find(|ty| ty.tokens().to_string() == f.ty.ident().to_string())
                })
            .collect();

            let delegate_generics: TokenStream = generics
                .iter()
                .map(|g| g.tokens())
                .map(|g| quote!($g,))
                .collect();

            let delegate = quote! {
                pub struct $delegate_name<$delegate_generics> {
                    $field_definitions
                }
            };

            let delegate_deserialize_impl =
                derive_deserialize(delegate.clone().into_iter().peekable(), &attributes);
            quote! {
                $delegate
                $delegate_deserialize_impl
                if let Some(__variant_match) = ::jsonette::from_slice::<$delegate_name<$delegate_generics>>(field) {
                    let $delegate_name::<$delegate_generics> {
                        $fields
                    } = __variant_match;
                    return Some(Self::$tag { $fields });
                }
            }
        })
        .collect();

    let try_struct: TokenStream = if !struct_variants.is_empty() {
        quote! {
            if parser.try_parse_object_start().is_some() {
                let field = parser.parse_element()?;
                $try_structs
                return None;
            }
        }
    } else {
        quote!()
    };

    let try_tuples: TokenStream = tuple_variants
        .iter()
        .map(|variant| {
            let tag = variant.tag.stream();
            let delegate_name: TokenStream = format!("__Delegate_{}", tag).parse().unwrap();

            let fields: TokenStream = (0..variant.fields.len())
                .map(|i| {
                    let name: TokenStream = format!("__field_{}", i).parse().unwrap();
                    quote!($name,)
                })
                .collect();

            let field_definitions: TokenStream = variant
                .fields
                .iter()
                .map(|field| {
                    let ty = field.ty.tokens();
                    quote!($ty,)
                })
                .collect();

            let generics: Vec<Generic> = variant
                .fields
                .iter()
                .filter_map(|f| {
                    all_generics.iter().cloned()
                        .find(|ty| ty.tokens().to_string() == f.ty.ident().to_string())
                })
            .collect();

            let delegate_generics: TokenStream = generics
                .iter()
                .map(|g| g.tokens())
                .map(|g| quote!($g,))
                .collect();

            let delegate = quote! {
                pub struct $delegate_name<$delegate_generics>($field_definitions);
            };
            let delegate_deserialize_impl =
                derive_deserialize(delegate.clone().into_iter().peekable(), attributes);

            quote! {
                $delegate
                $delegate_deserialize_impl
                if let Some(__variant_match) = ::jsonette::from_slice::<$delegate_name<$delegate_generics>>(field) {
                    let $delegate_name::<$delegate_generics>($fields) = __variant_match;
                    return Some(Self::$tag($fields));
                }
            }
        })
        .collect();

    let try_tuple: TokenStream = if !tuple_variants.is_empty() {
        quote! {
            if parser.try_parse_array_start().is_some() {
                let field = parser.parse_element()?;
                $try_tuples
                return None;
            }
        }
    } else {
        quote!()
    };

    let try_newtype: TokenStream = newtype_variants
        .iter()
        .map(|variant| {
            let tag = variant.tag.stream();
            let field = &variant.fields[0];
            let ty = field.ty.tokens_turbofish();
            quote! {
                if let Some(f) = ::jsonette::from_slice::<$ty>(field) {
                    return Some(Self::$tag(f));
                }
            }
        })
        .collect();

    let try_primitive: TokenStream = if let Some(variant) = primitive_variants.first() {
        let tag = variant.tag.stream();
        quote! {
            if field == b"null" {
                return Some(Self::$tag)
            }
        }
    } else {
        quote!()
    };

    quote! {
        impl<'de, $additional_generics> ::jsonette::Deserialize<'de> for $ty $where_clause {
            fn deserialize<R: ::jsonette::Read<'de>>(parser: &mut ::jsonette::Parser<R>) -> Option<Self> {
                $try_struct
                $try_tuple
                let field = parser.parse_element()?;
                $try_newtype
                $try_primitive
                None
            }
        }
    }
}

fn deserialize_repr_enum(mut tokens: Parser, _attributes: &Attributes) -> TokenStream {
    let ty = tokens.try_type().unwrap();
    let variants = tokens.try_group().unwrap().parser().try_repr_enum_fields();
    let additional_generics = ty.additional_generics();
    let where_clause = ty.where_clause();

    let matches: TokenStream = variants
        .into_iter()
        .map(|(tag, val)| {
            let tag = tag.into_stream();
            let val = val.into_stream();
            quote! {
                $val => Some(Self::$tag),
            }
        })
        .collect();

    quote! {
        impl<'de, $additional_generics> ::jsonette::Deserialize<'de> for $ty $where_clause {
            fn deserialize<R: ::jsonette::Read<'de>>(parser: &mut ::jsonette::Parser<R>) -> Option<Self> {
                let n = parser.parse_deserialize::<u8>()?;
                match n {
                    $matches
                    _ => None,
                }
            }
        }
    }
}

fn generate_deserialize_for_tuple_struct(
    struct_name: Type,
    field_group: Group,
    _attributes: &Attributes,
) -> TokenStream {
    let fields = field_group.parser().try_tuple_fields();
    let additional_generics = struct_name.additional_generics();
    let where_clause = struct_name.where_clause_deserialize();

    let parse_fields: TokenStream = fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let ty = field.ty.tokens();
            let name: TokenStream = format!("__field_{}", i).parse().unwrap();
            let deserialize_fn = match field.attributes.deserialize_with() {
                Some(tokens) => tokens,
                _ => quote!(::jsonette::Parser::<R>::parse_deserialize::<$ty>),
            };
            quote!(
                let $name = $deserialize_fn(parser)?;
            )
        })
        .collect();

    let build_fields: TokenStream = (0..fields.len())
        .map(|i| {
            let name: TokenStream = format!("__field_{}", i).parse().unwrap();
            quote!(
                $name,
            )
        })
        .collect();

    let (start, end) = if fields.len() == 1 {
        (quote!(), quote!())
    } else {
        (
            quote!(parser.parse_array_start()?;),
            quote!(parser.parse_array_end()?;),
        )
    };

    quote! {
        impl<'de, $additional_generics> ::jsonette::Deserialize<'de> for $struct_name $where_clause {
            fn deserialize<R: ::jsonette::Read<'de>>(parser: &mut ::jsonette::Parser<R>) -> Option<Self> {
                $start
                $parse_fields
                $end
                Some(Self($build_fields))
            }
        }
    }
}

fn generate_deserialize_for_named_struct(
    struct_name: Type,
    field_group: Group,
    attributes: &Attributes,
) -> TokenStream {
    let fields = field_group.parser().try_struct_fields();
    let additional_generics = struct_name.additional_generics();
    let where_clause = struct_name.where_clause_deserialize();

    let is_there_a_flattened_field = fields
        .iter()
        .any(|field| field.attributes.contains(&Attribute::Flatten));

    let partial_name = {
        let s = format!("__Partial_{}", struct_name.tokens_without_generics());
        let i = proc_macro::Ident::new(&s, proc_macro::Span::call_site()).into_stream();
        if is_there_a_flattened_field {
            quote!($i<'de, $additional_generics R>)
        } else {
            quote!($i<$additional_generics>)
        }
    };

    let make_partial = generate_partial(
        struct_name.clone(),
        partial_name.clone(),
        &fields,
        &attributes,
    );

    quote! {
        $make_partial
        impl<'de, $additional_generics> ::jsonette::Deserialize<'de> for $struct_name $where_clause {
            fn deserialize<R: ::jsonette::Read<'de>>(parser: &mut ::jsonette::Parser<R>) -> Option<Self> {
                let mut partial = <$partial_name as ::jsonette::Partial<'de, R>>::new();
                parser.parse_object_start()?;
                while let Some(field) = parser.try_parse_name() {
                    <$partial_name as ::jsonette::Partial<'de, R>>::try_parse(&mut partial, field, parser)?;
                }
                parser.parse_object_end()?;
                <$partial_name as ::jsonette::Partial<'de, R>>::build(partial)
            }
        }
    }
}

fn generate_partial(
    owner: Type,
    partial_name: TokenStream,
    fields: &[Field],
    attributes: &Attributes,
) -> TokenStream {
    let additional_generics = owner.additional_generics();
    let where_clause = owner.where_clause_deserialize();

    let normal_fields: Vec<Field> = fields
        .iter()
        .filter(|field| !field.attributes.contains(&Attribute::Flatten))
        .cloned()
        .collect();

    let delegate_fields: Vec<Field> = fields
        .iter()
        .filter(|field| field.attributes.contains(&Attribute::Flatten))
        .cloned()
        .collect();

    let partial_typename = {
        let s = format!("__Partial_{}", owner.tokens_without_generics());
        let i = proc_macro::Ident::new(&s, proc_macro::Span::call_site()).into_stream();
        if delegate_fields.is_empty() {
            quote!($i<$additional_generics>)
        } else {
            quote!($i<'de, $additional_generics R: ::jsonette::Read<'de>>)
        }
    };

    let match_and_parse: TokenStream = normal_fields
        .iter()
        .map(|field| {
            let ty = field.ty.tokens();
            let name = field.name.clone().unwrap().into_tree();

            let case = if attributes.contains(&Attribute::Camel) {
                let str = camel_case(&name.to_string());
                proc_macro::Ident::new(&str, proc_macro::Span::call_site()).into()
            } else {
                name.clone()
            };

            let deserialize_fn = match field.attributes.deserialize_with() {
                Some(tokens) => tokens,
                _ => quote!(::jsonette::Parser::<R>::parse_deserialize::<$ty>),
            };

            quote!(
                stringify!($case) => match self.$name {
                    None => self.$name = Some($deserialize_fn(parser)?),
                    Some(_) => return None,
                }
            )
        })
        .collect();

    let partial_fields: TokenStream = normal_fields
        .iter()
        .map(|field| {
            let ty = field.ty.tokens();
            let name = field.name.clone().unwrap().into_tree();
            quote!($name: Option<$ty>,)
        })
        .collect();

    let field_names: TokenStream = normal_fields
        .iter()
        .map(|field| {
            let name = field.name.clone().unwrap().into_tree();
            quote!($name,)
        })
        .collect();

    let partial_field_names: TokenStream = normal_fields
        .iter()
        .map(|field| {
            let name = field.name.clone().unwrap().into_tree();
            quote!($name: None,)
        })
        .collect();

    let partial_field_names_delegated: TokenStream = delegate_fields.iter().enumerate().map(|(i, field)| {
        let ty = field.ty.tokens();
        let partial_name = {
            let str = format!("__delegate_partial_{}", i);
            let name = proc_macro::Ident::new(&str, proc_macro::Span::call_site());
            name.stream()
        };
        quote!($partial_name: <<$ty as ::jsonette::DeserializePartial<'de, R>>::Partial as ::jsonette::Partial<'de, R>>::new(),)
    }).collect();

    let field_assignment: TokenStream = normal_fields
        .iter()
        .map(|field| {
            let name = field.name.clone().unwrap().into_tree();
            if field.attributes.contains(&Attribute::Optional)
                || field.attributes.contains(&Attribute::Default)
            {
                quote!($name: $name.unwrap_or_default(),)
            } else {
                quote!($name: $name?,)
            }
        })
        .collect();

    let field_assignment_delegated: TokenStream = delegate_fields.iter().enumerate().map(|(i, field)| {
        let name = field.name.clone().unwrap().into_tree();
        let ty = field.ty.tokens();
        let partial_name = {
            let str = format!("__delegate_partial_{}", i);
            let name = proc_macro::Ident::new(&str, proc_macro::Span::call_site());
            name.stream()
        };
        if field.attributes.contains(&Attribute::Optional) || field.attributes.contains(&Attribute::Default) {
            quote!($name: <<$ty as ::jsonette::DeserializePartial<'de, R>>::Partial as ::jsonette::Partial<'de, R>>::build($partial_name).unwrap_or_default(),)
        } else {
            quote!($name: <<$ty as ::jsonette::DeserializePartial<'de, R>>::Partial as ::jsonette::Partial<'de, R>>::build($partial_name)?,)
        }
    }).collect();

    let partial_fields_delegated: TokenStream = delegate_fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let ty = field.ty.tokens();
            let partial_name = {
                let str = format!("__delegate_partial_{}", i);
                let name = proc_macro::Ident::new(&str, proc_macro::Span::call_site());
                name.stream()
            };
            quote!(
                $partial_name: <$ty as ::jsonette::DeserializePartial<'de, R>>::Partial,
            )
        })
        .collect();

    let match_and_parse_delegated: TokenStream = delegate_fields.iter()
        .enumerate()
        .map(|(i, field)| {
            let ty = field.ty.tokens();
            let partial_name = {
                let str = format!("__delegate_partial_{}", i);
                let name = proc_macro::Ident::new(&str, proc_macro::Span::call_site());
                name.stream()
            };
            quote!(
                if <$ty as ::jsonette::DeserializePartial<'de, R>>::Partial::try_parse(&mut self.$partial_name, field, parser).is_some() {
                    return Some(());
                }
            )
        }).collect();

    let field_names_delegated: TokenStream = delegate_fields
        .iter()
        .enumerate()
        .map(|(i, _)| {
            let partial_name = {
                let str = format!("__delegate_partial_{}", i);
                let name = proc_macro::Ident::new(&str, proc_macro::Span::call_site());
                name.stream()
            };
            quote!($partial_name,)
        })
        .collect();

    quote! {
        impl<'de, $additional_generics R: ::jsonette::Read<'de>> ::jsonette::DeserializePartial<'de, R> for $owner $where_clause {
            type Partial = $partial_name;
        }

        pub struct $partial_typename {
            $partial_fields
            $partial_fields_delegated
        }

        impl<'de, $additional_generics R: ::jsonette::Read<'de>> ::jsonette::Partial<'de, R> for $partial_name $where_clause {
            type Full = $owner;

            fn new() -> Self {
                Self {
                    $partial_field_names
                    $partial_field_names_delegated
                }
            }

            #[allow(unreachable_code)]
            fn try_parse(&mut self, field: &'de str, parser: &mut ::jsonette::Parser<R>) -> Option<()> {
                match field {
                    $match_and_parse
                    _ => {
                        $match_and_parse_delegated
                        return None;
                    }
                }
                Some(())
            }

            fn build(self) -> Option<Self::Full> {
                let Self {
                    $field_names
                    $field_names_delegated
                } = self;
                Some(Self::Full {
                    $field_assignment
                    $field_assignment_delegated
                })
            }
        }
    }
}

fn derive_for_struct(mut tokens: Parser, attributes: Attributes) -> TokenStream {
    let struct_name = tokens.try_type().unwrap();
    let group = tokens.try_group().unwrap();

    let (kind, fields) = match group.delimiter() {
        Delimiter::Parenthesis => (VariantKind::Tuple, group.parser().try_tuple_fields()),
        Delimiter::Brace => (VariantKind::Struct, group.parser().try_struct_fields()),
        Delimiter::None => panic!("empty structs unsupported"),
        Delimiter::Bracket => unreachable!(),
    };

    let primitive_impl = match (kind, fields.len()) {
        (VariantKind::Tuple, 1) => {
            quote! {
                fn serialize_maybe_primitive<W: ::std::io::Write>(&self, w: &mut W) -> ::std::io::Result<()> {
                    self.serialize(w)
                }
            }
        }
        (VariantKind::Tuple, _) => {
            quote! {
                fn serialize_maybe_primitive<W: ::std::io::Write>(&self, w: &mut W) -> ::std::io::Result<()> {
                    self.serialize_array(w)
                }
            }
        }
        _ => quote!(),
    };

    let writes: TokenStream = fields
        .into_iter()
        .enumerate()
        .map(|(i, field)| {
            let variable = match field.name.as_ref().cloned() {
                Some(name) => {
                    let variable: TokenTree = name.into();
                    quote!(self.$variable)
                }
                None => {
                    let lit: TokenTree = Literal::usize_unsuffixed(i).into();
                    quote!(self.$lit)
                }
            };
            serialize_field(field, &attributes, i, variable)
        })
        .collect();

    let additional_generics = struct_name.additional_generics();
    let where_clause = struct_name.where_clause();

    quote! {
        impl<$additional_generics> jsonette::Serialize for $struct_name $where_clause {
            fn serialize<W: ::std::io::Write>(&self, w: &mut W) -> ::std::io::Result<()> {
                $writes
                Ok(())
            }
            $primitive_impl
        }
    }
}

fn derive_for_enum(mut tokens: Parser, attributes: Attributes) -> TokenStream {
    if attributes.contains(&Attribute::Repr) {
        return derive_for_repr(tokens);
    }

    let ty = tokens.try_type().unwrap();
    let tagged = !attributes.contains(&Attribute::Untagged);

    let variants = tokens.try_group().unwrap().parser().try_enum_variants();

    let writes: TokenStream = variants
        .iter()
        .map(|variant| serialize_variant(ty.clone(), variant, tagged))
        .collect();

    let enum_name = ty.tokens_turbofish();
    let primitive_impl: TokenStream = variants
        .into_iter()
        .map(|variant| {
            let tag = variant.tokens();
            match (variant.fields.len(), variant.kind) {
                (_, parse::VariantKind::Unit) => quote!($enum_name::$tag => self.serialize(w)?,),
                (_, parse::VariantKind::Struct) if tagged => {
                    quote!($enum_name::$tag => self.serialize_object(w)?,)
                }
                (_, parse::VariantKind::Struct) => quote!($enum_name::$tag => self.serialize(w)?,),
                (1, parse::VariantKind::Tuple) => quote!($enum_name::$tag => self.serialize(w)?,),
                (_, parse::VariantKind::Tuple) => {
                    quote!($enum_name::$tag => self.serialize_array(w)?,)
                }
            }
        })
        .collect();

    let additional_generics = ty.additional_generics();
    let where_clause = ty.where_clause();

    quote!(
        impl<$additional_generics> jsonette::Serialize for $ty $where_clause {
            #[allow(unused_variables)]
            fn serialize<W: ::std::io::Write>(&self, w: &mut W) -> ::std::io::Result<()> {
                match self {
                    $writes
                }
                Ok(())
            }
            #[allow(unused_variables)]
            fn serialize_maybe_primitive<W: ::std::io::Write>(&self, w: &mut W) -> ::std::io::Result<()> {
                match self {
                    $primitive_impl
                }
                Ok(())
            }
        }
    )
}

fn derive_for_repr(mut tokens: Parser) -> TokenStream {
    let ty = tokens.try_type().unwrap();
    let fields = tokens.try_group().unwrap().parser().try_repr_enum_fields();

    let ts: TokenStream = fields
        .into_iter()
        .map(|(name, val)| {
            let name: TokenTree = name.into();
            let val: TokenTree = Literal::byte_string(val.to_string().as_bytes()).into();
            quote!($ty::$name => $val,)
        })
        .collect();

    let additional_generics = ty.additional_generics();
    let where_clause = ty.where_clause();

    quote!(
        impl<$additional_generics> jsonette::Serialize for $ty $where_clause {
            fn serialize<W: ::std::io::Write>(&self, w: &mut W) -> ::std::io::Result<()> {
                w.write_all(match self {
                    $ts
                })
            }
            #[inline]
            fn serialize_maybe_primitive<W: ::std::io::Write>(&self, w: &mut W) -> ::std::io::Result<()> {
                self.serialize(w)
            }
        }
    )
}

fn serialize_field(
    Field {
        name,
        ty,
        attributes,
    }: Field,
    global_attributes: &Attributes,
    n: usize,
    variable: TokenStream,
) -> TokenStream {
    let write_name = match name {
        Some(name) => {
            let ident: TokenTree = name.into();
            let ident_str = if global_attributes.contains(&Attribute::Camel) {
                camel_case(&ident.to_string())
            } else {
                ident.to_string()
            };
            let ident_in_bytes = format!("{:?}", ident_str.to_string().as_bytes());
            let ident_in_bytes: TokenStream = ident_in_bytes.parse().unwrap();
            quote!(
                w.write_all(b"\"")?;
                w.write_all(&$ident_in_bytes)?;
                w.write_all(b"\":")?;
            )
        }
        None => quote!(),
    };
    // }

    let mut write_code = if n != 0 {
        quote!(w.write_all(b",")?;)
    } else {
        quote!()
    };

    let wrap_start = if attributes.contains(&Attribute::Flatten) {
        quote!()
    } else {
        quote!($write_name)
    };

    write_code.extend(match &*ty.tokens().to_string() {
        _ if attributes.serialize_with().is_some() => {
            let serialize_fn = attributes.serialize_with().unwrap();

            quote!(
                $wrap_start
                $serialize_fn(&$variable, &mut *w)?;
            )
        }
        _ if attributes.contains(&Attribute::Flatten) => {
            quote!(
                $variable.serialize(&mut *w)?;
            )
        }
        _ => quote!(
            $wrap_start
            $variable.serialize_maybe_primitive(&mut *w)?;
        ),
    });

    if attributes.contains(&Attribute::Optional) {
        quote!(
            if ::core::option::Option::is_some(&$variable) {
                $write_code
            }
        )
    } else {
        write_code
    }
}

fn serialize_variant(enum_name: Type, variant: &Variant, tagged: bool) -> TokenStream {
    let enum_name = enum_name.tokens_turbofish();
    let tag = variant.tokens();
    if variant.fields.is_empty() {
        if tagged {
            quote!(
                $enum_name::$tag => {
                    w.write_all(b"\"")?;
                    w.write_all(stringify!($tag).as_bytes())?;
                    w.write_all(b"\"")?;
                }
            )
        } else {
            quote!(
                $enum_name::$tag => w.write_all("null".as_bytes())?,
            )
        }
    } else {
        let tt: TokenStream = variant
            .fields
            .iter()
            .zip(variant.field_tokens())
            .map(|(field, variable)| {
                let name = tagged.then(|| variant.tag.clone());
                serialize_field(
                    Field {
                        name,
                        ty: field.ty.clone(),
                        attributes: Attributes::default(),
                    },
                    &Attributes::default(),
                    0,
                    variable,
                )
            })
            .collect();
        quote!(
            $enum_name::$tag => { $tt }
        )
    }
}

fn camel_case(s: &str) -> String {
    fn capitalise_beginning(s: &str) -> impl Iterator<Item = char> + '_ {
        s.chars()
            .flat_map(char::to_uppercase)
            .take(1)
            .chain(s.chars().skip(1).flat_map(char::to_lowercase))
    }

    s.split('_')
        .take(1)
        .flat_map(str::chars)
        .flat_map(char::to_lowercase)
        .chain(s.split('_').skip(1).flat_map(capitalise_beginning))
        .collect()
}
