use jsonette::{Deserialize, Serialize, Value};
use std::{borrow::Cow, collections::HashMap};

#[test]
fn deserialize_u32() {
    let expected = 5;
    let input = r#"5"#;
    let output: u32 = jsonette::from_slice::<u32>(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_struct() {
    #[derive(Debug, PartialEq, Deserialize)]
    pub struct Foo {
        foo: u32,
        bar: u64,
    }

    let expected = Foo { foo: 5, bar: 7 };
    let input = r#"{
        "foo": 5,
        "bar": 7,
    }"#;
    let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();

    assert_eq!(output, expected);
}

#[test]
fn deserialize_tuple_struct() {
    #[derive(Debug, PartialEq, Deserialize)]
    pub struct Foo(u32, u64);

    let expected = Foo(5, 7);
    let input = r#"[5, 7]"#;
    let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_enum() {
    #[derive(Debug, PartialEq, Deserialize)]
    pub enum Foo {
        Foo,
        Bar,
    }

    let expected = Foo::Bar;
    let input = r#""Bar""#;
    let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_enum_with_struct_variant() {
    #[derive(Debug, PartialEq, Deserialize)]
    pub enum Foo {
        Foo,
        Bar { foo: u32, bar: u64 },
    }

    let expected = Foo::Bar { foo: 7, bar: 5 };
    let input = r#"{ "Bar": { "foo": 7, "bar": 5 }}"#;
    let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_camel() {
    #[derive(Debug, PartialEq, Deserialize)]
    #[jsonette(camel)]
    pub enum Foo {
        FooBar,
        BarFoo { foo_bar: u32, bar_foo: u64 },
    }

    let expected = Foo::BarFoo {
        foo_bar: 7,
        bar_foo: 5,
    };
    let input = r#"{ "barFoo": { "fooBar": 7, "barFoo": 5 }}"#;
    let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_enum_with_tuple_variant() {
    #[derive(Debug, PartialEq, Deserialize)]
    pub enum Foo {
        Foo,
        Bar(u32, u64),
    }

    let expected = Foo::Bar(5, 7);
    let input = r#"{ "Bar": [5, 7] }"#;
    let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_enum_with_newtype_variant() {
    #[derive(Debug, PartialEq, Deserialize)]
    pub enum Foo {
        Foo,
        Bar(u32),
    }

    let expected = Foo::Bar(5);
    let input = r#"{ "Bar": 5 }"#;
    let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_enum_with_generics() {
    #[derive(Debug, PartialEq, Deserialize)]
    pub enum Foo<A, B, C> {
        Foo(C),
        Bar { foo: A, bar: B },
        Baz(A, B),
    }

    let expected = Foo::<u32, u64, usize>::Foo(12);
    let input = r#"{ "Foo": 12 }"#;
    let output: Foo<u32, u64, usize> = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);

    let expected = Foo::<u32, u64, usize>::Bar { foo: 7, bar: 5 };
    let input = r#"{ "Bar": { "foo": 7, "bar": 5 }}"#;
    let output: Foo<u32, u64, usize> = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);

    let expected = Foo::<u32, u64, usize>::Baz(7, 5);
    let input = r#"{ "Baz": [7, 5] }"#;
    let output: Foo<u32, u64, usize> = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_untagged_enum_with_generics() {
    #[derive(Debug, PartialEq, Deserialize)]
    #[jsonette(untagged)]
    pub enum Foo<A, B, C> {
        Foo(C),
        Bar { foo: A, bar: B },
        Baz(A, B),
    }

    let expected = Foo::<u32, u64, usize>::Foo(12);
    let input = r#"12"#;
    let output: Foo<u32, u64, usize> = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);

    let expected = Foo::<u32, u64, usize>::Bar { foo: 7, bar: 5 };
    let input = r#"{ "foo": 7, "bar": 5 }"#;
    let output: Foo<u32, u64, usize> = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);

    let expected = Foo::<u32, u64, usize>::Baz(7, 5);
    let input = r#"[7, 5]"#;
    let output: Foo<u32, u64, usize> = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
#[allow(dead_code)]
fn deserialize_untagged_enum() {
    #[derive(Debug, PartialEq, Deserialize)]
    #[jsonette(untagged)]
    pub enum Foo {
        Foo,
        Bar,
    }

    let expected = Foo::Foo;
    let input = r#"null"#;
    let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_untagged_enum_with_struct_variant() {
    #[derive(Debug, PartialEq, Deserialize)]
    #[jsonette(untagged)]
    pub enum Foo {
        Foo,
        Bar { foo: u32, bar: u64 },
        Baz { foo: u32, bar: String },
    }

    let expected = Foo::Bar { foo: 5, bar: 7 };
    let input = r#"{ "foo": 5, "bar": 7 }"#;
    let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_untagged_enum_with_tuple_variant() {
    #[derive(Debug, PartialEq, Deserialize)]
    #[jsonette(untagged)]
    pub enum Foo {
        Foo,
        Bar(u32, u64),
    }

    let expected = Foo::Bar(5, 7);
    let input = r#"[5, 7]"#;
    let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_untagged_enum_with_newtype_variant() {
    #[derive(Debug, PartialEq, Deserialize)]
    #[jsonette(untagged)]
    pub enum Foo {
        Foo,
        Bar(u32),
    }

    let expected = Foo::Bar(5);
    let input = r#"5"#;
    let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_repr_enum() {
    #[derive(Debug, PartialEq, Deserialize)]
    #[repr(u8)]
    pub enum Foo {
        Foo = 5,
        Bar = 7,
    }

    let expected = Foo::Bar;
    let input = r#"7"#;
    let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_arbitrary_type_nesting() {
    type ExpectedType<'a> = Vec<Box<HashMap<Cow<'a, str>, usize>>>;
    let expected: ExpectedType = vec![Box::new(
        std::array::IntoIter::new([(Cow::from("foo"), 5), (Cow::from("bar"), 7)])
            .collect::<HashMap<Cow<'_, str>, usize>>(),
    )];
    let input: &'static str = r#"[{"foo": 5, "bar": 7}]"#;
    let output: ExpectedType = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_struct_with_generics() {
    #[derive(Debug, PartialEq, Deserialize)]
    pub struct Foo<A, B> {
        foo: A,
        bar: B,
    }

    let expected = Foo::<u32, u64> { foo: 5, bar: 7 };
    let input = r#"{
        "foo": 5,
        "bar": 7,
    }"#;
    let output: Foo<u32, u64> = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn deserialize_struct_with_newtype_cow() {
    #[derive(Debug, PartialEq, Deserialize)]
    pub struct Foo(Cow<'static, str>);

    let expected = Foo(Cow::Owned("foo".to_string()));
    let input = r#""foo""#;
    let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();
    assert_eq!(output, expected);
}

macro_rules! test_serialize {
    ($input:expr, $expected:ident) => {
        let mut output: Vec<u8> = Vec::new();
        $input.serialize_maybe_primitive(&mut output).unwrap();
        let output = String::from_utf8(output).unwrap();
        assert_eq!(output, $expected);
    };
}

#[test]
fn serialize_struct() {
    #[derive(Debug, PartialEq, Serialize)]
    pub struct Foo {
        foo: u32,
        bar: u64,
    }

    let expected = r#"{"foo":5,"bar":7}"#;
    let input = Foo { foo: 5, bar: 7 };
    test_serialize!(input, expected);
}

#[test]
fn serialize_newtype_struct() {
    #[derive(Debug, PartialEq, Serialize)]
    pub struct Foo(u32);

    let expected = r#"5"#;
    let input = Foo(5);
    test_serialize!(input, expected);
}

#[test]
fn serialize_tuple_struct() {
    #[derive(Debug, PartialEq, Serialize)]
    pub struct Foo(u32, u64);

    let expected = r#"[5,7]"#;
    let input = Foo(5, 7);
    test_serialize!(input, expected);
}

#[test]
fn serialize_struct_with_borrow() {
    #[derive(Debug, PartialEq, Serialize)]
    pub struct Foo<'a>(Value<'a>);

    let expected = r#"[5,7]"#;
    let input = Value(br#"[5,7]"#);
    test_serialize!(input, expected);
}

// #[test]
// fn deserialize_struct_with_borrow() {
//     // do we just replace all lifetimes with 'de?
//     #[derive(Debug, PartialEq, Deserialize)]
//     pub struct Foo<'de>(Value<'de>);

//     let expected = Foo(Value(br#""foo""#));
//     let input = r#""foo""#;
//     let output: Foo = jsonette::from_slice(input.as_bytes()).unwrap();
//     assert_eq!(output, expected);
// }
