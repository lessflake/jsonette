use proc_macro::{
    quote, token_stream, Delimiter, Group, Ident, Literal, Punct, TokenStream, TokenTree,
};
use std::iter::Peekable;

pub type Parser = Peekable<token_stream::IntoIter>;

pub trait TokenExt: Clone {
    fn into_stream(self) -> TokenStream;
    fn into_tree(self) -> TokenTree;

    fn stream(&self) -> TokenStream {
        self.clone().into_stream()
    }
    fn tree(&self) -> TokenTree {
        self.clone().into_tree()
    }
    fn parser(self) -> Parser {
        self.into_stream().into_iter().peekable()
    }
}

impl TokenExt for Group {
    fn into_stream(self) -> TokenStream {
        self.stream()
    }
    fn into_tree(self) -> TokenTree {
        self.into()
    }
}

macro_rules! impl_token_ext {
    ($t:ty) => {
        impl TokenExt for $t {
            fn into_stream(self) -> TokenStream {
                self.into_tree().into()
            }
            fn into_tree(self) -> TokenTree {
                self.into()
            }
        }
    };
}

impl_token_ext!(Ident);
impl_token_ext!(Punct);
impl_token_ext!(Literal);

pub trait ParseExt {
    fn try_ident(&mut self) -> Option<Ident>;
    fn try_punct(&mut self) -> Option<char>;
    fn try_group(&mut self) -> Option<Group>;
    fn try_attribute(&mut self) -> Option<Attributes>;
    fn try_attributes(&mut self) -> Attributes;
    fn try_literal(&mut self) -> Option<Literal>;
    fn try_type(&mut self) -> Option<Type>;
    fn try_repr_enum_fields(&mut self) -> Vec<(Ident, Literal)>;
    fn try_struct_fields(&mut self) -> Vec<Field>;
    fn try_tuple_fields(&mut self) -> Vec<Field>;
    fn try_enum_variants(&mut self) -> Vec<Variant>;
    fn peek_punct(&mut self) -> Option<char>;
    fn peek_group(&mut self) -> Option<Group>;
    fn peek_ident(&mut self) -> Option<Ident>;
    fn eat_generic_parameters(&mut self) -> Vec<Generic>;
    fn eat_generic_constraints(&mut self) -> Option<TokenStream>;
    fn eat_visibility(&mut self);
}

impl<I> ParseExt for Peekable<I>
where
    I: Iterator<Item = TokenTree>,
{
    fn try_ident(&mut self) -> Option<Ident> {
        if let Some(TokenTree::Ident(ident)) = self.peek() {
            let ident = ident.clone();
            self.next();
            Some(ident)
        } else {
            None
        }
    }

    fn try_punct(&mut self) -> Option<char> {
        if let Some(TokenTree::Punct(punct)) = self.peek() {
            let punct = punct.as_char();
            self.next();
            Some(punct)
        } else {
            None
        }
    }

    fn try_group(&mut self) -> Option<Group> {
        if let Some(TokenTree::Group(group)) = self.peek() {
            let group = group.clone();
            self.next();
            Some(group)
        } else {
            None
        }
    }

    fn try_attributes(&mut self) -> Attributes {
        std::iter::from_fn(|| self.try_attribute())
            .flatten()
            .collect()
    }

    fn try_attribute(&mut self) -> Option<Attributes> {
        let mut attributes = Attributes::default();

        if self.peek_punct() != Some('#') {
            return None;
        }
        self.next().unwrap();
        let mut group = self.try_group().expect("attribute outer group").parser();

        match group.try_ident().map(|i| i.to_string()).as_deref() {
            Some("repr") => {
                if &*group.try_group()?.stream().to_string() == "u8" {
                    attributes.push(Attribute::Repr);
                }
                return Some(attributes);
            }
            Some("jsonette") => {}
            _ => return Some(attributes),
        }

        let mut group = group.try_group().expect("attribute inner group").parser();

        loop {
            let attribute_ident = match group.try_ident() {
                Some(x) => x,
                _ => return Some(attributes),
            };
            let attr = match &*attribute_ident.to_string() {
                "optional" => Attribute::Optional,
                "camel" => Attribute::Camel,
                "flatten" => Attribute::Flatten,
                "untagged" => Attribute::Untagged,
                "default" => Attribute::Default,
                // "repr" => Attribute::Repr,
                "with" if group.try_punct().expect("with") == '=' => {
                    // reparse from literal
                    let lit = group.try_literal().unwrap().to_string();
                    let lit = &lit[1..lit.len() - 1];
                    let tokens = lit.parse::<TokenStream>().unwrap();
                    Attribute::With(tokens)
                }
                "deserialize_with" if group.try_punct().expect("de_with") == '=' => {
                    // reparse from literal
                    let lit = group.try_literal().unwrap().to_string();
                    let lit = &lit[1..lit.len() - 1];
                    let tokens = lit.parse::<TokenStream>().unwrap();
                    Attribute::DeserializeWith(tokens)
                }
                "serialize_with" if group.try_punct().expect("ser_with") == '=' => {
                    // reparse from literal
                    let lit = group.try_literal().unwrap().to_string();
                    let lit = &lit[1..lit.len() - 1];
                    let tokens = lit.parse::<TokenStream>().unwrap();
                    Attribute::SerializeWith(tokens)
                }
                _ => panic!("unrecognised attribute"),
            };
            attributes.push(attr);

            if !matches!(group.try_punct(), Some(',')) {
                break;
            }
        }

        Some(attributes)
    }

    fn try_literal(&mut self) -> Option<Literal> {
        if let Some(TokenTree::Literal(literal)) = self.peek() {
            let literal = literal.clone();
            self.next();
            Some(literal)
        } else {
            None
        }
    }

    fn try_type(&mut self) -> Option<Type> {
        match self.peek().clone() {
            Some(TokenTree::Ident(name)) => {
                let name = name.clone();
                self.next().unwrap();
                let generics = self.eat_generic_parameters();
                let constraints = self.eat_generic_constraints();
                Some(Type::Standard { name, generics, constraints })
            }
            Some(TokenTree::Punct(p)) if p.as_char() == '&' => {
                self.next().unwrap();
                // get rid of lifetime
                if self.peek_punct().filter(|&p| p == '\'').is_some() {
                    self.next().unwrap();
                    let _name = self.try_ident().expect("no ident after `'`?");
                }
                let ty = Box::new(self.try_type().unwrap());
                Some(Type::Ref { ty })
            }
            Some(TokenTree::Group(g)) => {
                let mut tokens = g.stream().into_iter().peekable();
                self.next().unwrap();
                let ty = Box::new(tokens.try_type().unwrap());
                let maybe_count = (tokens.peek_punct() == Some(';')).then(|| {
                    tokens.next().unwrap();
                    let count = tokens.try_literal().unwrap();
                    count.to_string().parse::<usize>().unwrap()
                });

                Some(match maybe_count {
                    Some(count) => Type::Array { ty, count },
                    None => Type::Slice { ty },
                })
            }
            _ => None,
        }
    }

    fn try_repr_enum_fields(&mut self) -> Vec<(Ident, Literal)> {
        let mut fields = Vec::new();

        if self.peek().is_none() {
            return fields;
        }

        loop {
            let _ = self.try_attributes();
            let name = self.try_ident().expect("missing name");
            self.try_punct().expect("missing = after name");

            let val = self.try_literal().expect("missing = after name");
            fields.push((name, val));

            let _ = self.try_punct();

            if self.peek().is_none() {
                break;
            }
        }
        fields
    }

    fn try_struct_fields(&mut self) -> Vec<Field> {
        let mut fields = Vec::new();

        if self.peek().is_none() {
            return fields;
        }

        while self.peek().is_some() {
            let attributes = self.try_attributes();
            self.eat_visibility();
            let name = self.try_ident().expect("missing name");
            self.try_punct()
                .filter(|&p| p == ':')
                .expect("missing : after name");

            let ty = self.try_type().unwrap();

            fields.push(Field {
                name: Some(name),
                ty,
                attributes,
            });

            let _ = self.try_punct();
        }
        fields
    }

    fn try_enum_variants(&mut self) -> Vec<Variant> {
        let mut variants = Vec::new();
        loop {
            let _ = self.try_attributes();

            let tag = self.try_ident().expect("missing name");

            let (kind, fields) = match self.try_group() {
                Some(g) => match g.delimiter() {
                    Delimiter::Brace => (VariantKind::Struct, g.parser().try_struct_fields()),
                    Delimiter::Parenthesis => (VariantKind::Tuple, g.parser().try_tuple_fields()),
                    _ => unreachable!(),
                },
                None => (VariantKind::Unit, Vec::new()),
            };

            variants.push(Variant { kind, tag, fields });

            let _ = self.try_punct();

            if self.peek().is_none() {
                break;
            }
        }
        variants
    }

    fn try_tuple_fields(&mut self) -> Vec<Field> {
        let mut fields = Vec::new();

        if self.peek().is_none() {
            return fields;
        }

        loop {
            let attributes = self.try_attributes();
            self.eat_visibility();
            let ty = self.try_type().unwrap();

            fields.push(Field {
                name: None,
                ty,
                attributes,
            });

            let _ = self.try_punct();

            if self.peek().is_none() {
                break;
            }
        }
        fields
    }

    fn peek_punct(&mut self) -> Option<char> {
        if let Some(TokenTree::Punct(punct)) = self.peek() {
            Some(punct.as_char())
        } else {
            None
        }
    }

    fn peek_group(&mut self) -> Option<Group> {
        if let Some(TokenTree::Group(group)) = self.peek() {
            Some(group.clone())
        } else {
            None
        }
    }

    fn peek_ident(&mut self) -> Option<Ident> {
        if let Some(TokenTree::Ident(ident)) = self.peek() {
            Some(ident.clone())
        } else {
            None
        }
    }

    fn eat_generic_parameters(&mut self) -> Vec<Generic> {
        let mut generics: Vec<Generic> = Vec::new();
        if let Some('<') = self.peek_punct() {
            let _ = self.next();
            loop {
                match self.peek_punct() {
                    Some('\'') => {
                        let _ = self.next();
                        let name = self.try_ident().expect("no ident after `'`?");
                        generics.push(Generic::Lifetime(name));
                    }

                    Some('>') => {
                        let _ = self.next();
                        break;
                    }
                    // Some(',') => break,

                    None => {
                        let ty = self.try_type().expect("expected generic");
                        generics.push(Generic::Type(ty));
                    }

                    Some(_) => {
                        unreachable!("invalid syntax in generics? 1");
                    }
                }

                match self.try_punct().unwrap() {
                    '>' => break,
                    ',' => continue,
                    _ => unreachable!("invalid syntax in generics? 2"),
                }
            }
        }

        generics
    }

    fn eat_generic_constraints(&mut self) -> Option<TokenStream> {
        if let Some("where") = self.peek_ident().map(|i| i.to_string()).as_deref() {
            let _ = self.next();
            let mut depth = 0;
            let mut tokens = TokenStream::default();
            loop {
                match self.peek()? {
                    TokenTree::Group(_) if depth == 0 => break,
                    TokenTree::Punct(p) if p.as_char() == '<' => depth += 1,
                    TokenTree::Punct(p) if p.as_char() == '>' => depth -= 1,
                    _ => {}
                }
                tokens.extend(self.next().into_iter());
            }
            Some(tokens)
        } else {
            None
        }
    }

    fn eat_visibility(&mut self) {
        if let Some("pub") = self.peek_ident().map(|i| i.to_string()).as_deref() {
            self.next().unwrap();
            if self.peek_group().is_some() {
                self.next().unwrap();
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Standard { name: Ident, generics: Vec<Generic>, constraints: Option<TokenStream> },
    Array { ty: Box<Type>, count: usize },
    Slice { ty: Box<Type> },
    Ref { ty: Box<Type> },
}

impl Type {
    pub fn ident(&self) -> Ident {
        match self {
            Type::Standard { name, .. } => name.clone(),
            Type::Array { ty, .. } => ty.ident(),
            Type::Slice { ty } => ty.ident(),
            Type::Ref { ty } => ty.ident(),
        }
    }

    pub fn generics(&self) -> &[Generic] {
        match self {
            Type::Standard { generics, .. } => generics,
            Type::Array { ty, .. } => ty.generics(),
            Type::Slice { ty } => ty.generics(),
            Type::Ref { ty } => ty.generics(),
        }
    }

    fn generics_tokens(&self) -> TokenStream {
        match self {
            Type::Standard { generics, .. } => generics
                .iter()
                .map(|g| g.tokens())
                .map(|tt| quote!($tt,))
                .collect(),
            _ => panic!("called generics_tokens() on non-standard type"),
        }
    }

    pub fn tokens(&self) -> TokenStream {
        match self {
            Type::Standard { name, generics, .. } => {
                let name: TokenTree = name.clone().into();
                if generics.is_empty() {
                    quote!($name)
                } else {
                    let generics = self.generics_tokens();
                    quote!($name<$generics>)
                }
            }
            Type::Array { ty, count } => {
                let ty = ty.tokens();
                let count: TokenTree = Literal::usize_unsuffixed(*count).into();
                quote!([$ty;$count])
            }
            Type::Slice { ty } => {
                let ty = ty.tokens();
                quote!([$ty])
            }
            Type::Ref { ty } => {
                let ty = ty.tokens();
                quote!(&$ty)
            }
        }
    }

    pub fn tokens_without_generics(&self) -> TokenStream {
        match self {
            Type::Standard { name, .. } => {
                let name: TokenTree = name.clone().into();
                quote!($name)
            }
            Type::Array { ty, count } => {
                let ty = ty.tokens_without_generics();
                let count: TokenTree = Literal::usize_unsuffixed(*count).into();
                quote!([$ty;$count])
            }
            Type::Slice { ty } => {
                let ty = ty.tokens_without_generics();
                quote!([$ty])
            }
            Type::Ref { ty } => {
                let ty = ty.tokens_without_generics();
                quote!(&$ty)
            }
        }
    }

    pub fn tokens_turbofish(&self) -> TokenStream {
        match self {
            Type::Standard { name, generics, .. } => {
                let name: TokenTree = name.clone().into();
                if generics.is_empty() {
                    quote!($name)
                } else {
                    let generics = self.generics_tokens();
                    quote!($name::<$generics>)
                }
            }
            _ => self.tokens(),
        }
    }

    pub fn where_clause(&self) -> TokenStream {
        match self {
            Type::Standard { generics, .. } => {
                let bounds: TokenStream = generics
                    .iter()
                    .filter_map(Generic::bounded)
                    .map(|bound| quote!($bound,))
                    .collect();
                if bounds.clone().into_iter().count() == 0 {
                    quote!()
                } else {
                    quote!(where $bounds)
                }
            }
            Type::Array { ty, .. } => ty.where_clause(),
            Type::Slice { ty } => ty.where_clause(),
            Type::Ref { ty } => ty.where_clause(),
        }
    }

    pub fn where_clause_deserialize(&self) -> TokenStream {
        match self {
            Type::Standard { generics, .. } => {
                let bounds: TokenStream = generics
                    .iter()
                    .filter_map(Generic::bounded_deserialize)
                    .map(|bound| quote!($bound,))
                    .collect();
                if bounds.clone().into_iter().count() == 0 {
                    quote!()
                } else {
                    quote!(where $bounds)
                }
            }
            Type::Array { ty, .. } => ty.where_clause_deserialize(),
            Type::Slice { ty } => ty.where_clause_deserialize(),
            Type::Ref { ty } => ty.where_clause_deserialize(),
        }
    }

    pub fn additional_generics(&self) -> TokenStream {
        match self {
            Type::Standard { generics, .. } => generics
                .iter()
                .map(|g| {
                    let tokens = g.tokens();
                    quote!($tokens,)
                })
                .collect(),
            Type::Array { ty, .. } => ty.additional_generics(),
            Type::Slice { ty } => ty.additional_generics(),
            Type::Ref { ty } => ty.additional_generics(),
        }
    }
}

impl Into<TokenStream> for Type {
    fn into(self) -> TokenStream {
        self.tokens()
    }
}

#[derive(Debug, Clone)]
pub enum Generic {
    Lifetime(Ident),
    Type(Type),
}

impl Generic {
    pub fn tokens(&self) -> TokenStream {
        match self {
            Generic::Lifetime(ident) => {
                let apostrophe: TokenTree = Punct::new('\'', proc_macro::Spacing::Joint).into();
                let ident: TokenTree = ident.clone().into();
                quote!($apostrophe$ident)
            }
            Generic::Type(ty) => ty.tokens(),
        }
    }

    pub fn bounded(&self) -> Option<TokenStream> {
        match self {
            Generic::Lifetime(_) => None,
            Generic::Type(ty) => {
                let tokens = ty.tokens();
                Some(quote!($tokens: jsonette::Serialize<W>))
            }
        }
    }

    pub fn bounded_deserialize(&self) -> Option<TokenStream> {
        match self {
            Generic::Lifetime(_) => None,
            Generic::Type(ty) => {
                let tokens = ty.tokens();
                Some(quote!($tokens: jsonette::Deserialize<'de>))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Option<Ident>,
    pub ty: Type,
    pub attributes: Attributes,
}

#[derive(Debug, Clone)]
pub enum Attribute {
    Camel,
    Flatten,
    Optional,
    Untagged,
    Default,
    Repr,
    With(TokenStream),
    DeserializeWith(TokenStream),
    SerializeWith(TokenStream),
}

impl Attribute {
    fn matches(&self, other: &Attribute) -> bool {
        use std::mem::discriminant;
        discriminant(self) == discriminant(other)
    }
}

#[derive(Debug, Default, Clone)]
pub struct Attributes(Vec<Attribute>);

impl Attributes {
    fn push(&mut self, attr: Attribute) {
        if self.contains(&attr) {
            panic!("duplicate attribute: {:?}", attr);
        }
        self.0.push(attr)
    }

    pub fn contains(&self, attr: &Attribute) -> bool {
        self.0.iter().any(|a| a.matches(attr))
    }

    // TODO: this is janky and there HAS to be a better way
    pub fn has_with(&self) -> Option<TokenStream> {
        self.0
            .iter()
            .find(|a| a.matches(&Attribute::With(quote!())))
            .map(|a| match a {
                Attribute::With(tokens) => tokens,
                _ => unreachable!(),
            })
            .cloned()
    }

    pub fn serialize_with(&self) -> Option<TokenStream> {
        self.0
            .iter()
            .find(|a| a.matches(&Attribute::SerializeWith(quote!())))
            .map(|a| match a {
                Attribute::SerializeWith(tokens) => tokens,
                _ => unreachable!(),
            })
            .cloned()
            .or_else(|| self.has_with().map(|tokens| quote!($tokens::serialize)))
    }

    pub fn deserialize_with(&self) -> Option<TokenStream> {
        self.0
            .iter()
            .find(|a| a.matches(&Attribute::DeserializeWith(quote!())))
            .map(|a| match a {
                Attribute::DeserializeWith(tokens) => tokens,
                _ => unreachable!(),
            })
            .cloned()
            .or_else(|| self.has_with().map(|tokens| quote!($tokens::deserialize)))
    }

    pub fn camel(&self) -> bool {
        self.contains(&Attribute::Camel)
    }
}

impl IntoIterator for Attributes {
    type Item = Attribute;

    type IntoIter = std::vec::IntoIter<Attribute>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl std::iter::FromIterator<Attribute> for Attributes {
    fn from_iter<T: IntoIterator<Item = Attribute>>(iter: T) -> Self {
        Self(Vec::from_iter(iter))
    }
}

#[derive(Debug)]
pub enum VariantKind {
    Unit,
    Tuple,
    Struct,
}

#[derive(Debug)]
pub struct Variant {
    pub kind: VariantKind,
    pub tag: Ident,
    pub fields: Vec<Field>,
}

impl Variant {
    pub fn tokens(&self) -> TokenStream {
        let tag: TokenTree = self.tag.clone().into();
        let fields: TokenStream = self.field_tokens().map(|tt| quote!($tt,)).collect();
        match self.kind {
            VariantKind::Unit => quote!($tag),
            VariantKind::Tuple => quote!($tag($fields)),
            VariantKind::Struct => quote!($tag{$fields}),
        }
    }

    pub fn field_tokens(&self) -> impl Iterator<Item = TokenStream> + '_ {
        let tag = self.tag.clone();
        self.fields
            .iter()
            .cloned()
            .enumerate()
            .map(move |(i, f)| match f.name {
                Some(name) => TokenTree::from(name).into(),
                None => {
                    let s = format!("__jsonette_{}_{}", tag, i);
                    let i = Ident::new(&s, proc_macro::Span::call_site());
                    TokenTree::from(i).into()
                }
            })
    }

    pub fn tag_literal(&self, attributes: &Attributes) -> TokenStream {
        if !attributes.camel() {
            Literal::string(&*self.tag.to_string()).stream()
        } else {
            Literal::string(&*pascal_to_camel_case(&*self.tag.to_string())).stream()
        }
    }
}

// just lowercase the first letter
fn pascal_to_camel_case(s: &str) -> String {
    s.chars()
        .flat_map(char::to_lowercase)
        .take(1)
        .chain(s.chars().skip(1))
        .collect()
}
