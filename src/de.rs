use std::{borrow::Cow, collections::HashMap, hash::Hash};

use crate::Value;

pub trait Deserialize<'de>
where
    Self: Sized,
{
    fn deserialize<R: Read<'de>>(r: &mut Parser<R>) -> Option<Self>;
}

impl<'de, R> Parser<R>
where
    R: Read<'de>,
{
    pub fn next(&mut self) -> Option<u8> {
        self.input.next()
    }

    pub fn peek(&mut self) -> Option<u8> {
        self.input.peek()
    }

    pub fn advance(&mut self) {
        loop {
            match self.peek() {
                Some(b' ') => drop(self.next()),
                Some(b',') => drop(self.next()),
                Some(b'\n') => drop(self.next()),
                Some(b'\r') => drop(self.next()),
                _ => return,
            }
        }
    }

    pub fn parse_byte(&mut self, b: u8) -> Option<u8> {
        let value = match self.next() {
            Some(x) if x == b => Some(x),
            _ => None,
        };
        self.advance();
        value
    }

    pub fn peek_byte(&mut self, b: u8) -> Option<u8> {
        match self.peek() {
            Some(x) if x == b => Some(x),
            _ => None,
        }
    }

    pub fn try_parse_byte(&mut self, b: u8) -> Option<u8> {
        let value = match self.peek() {
            Some(x) if x == b => Some(x),
            _ => None,
        };
        value
    }

    pub fn parse_str(&mut self) -> Option<&'de str> {
        self.input.parse_str()
    }

    pub fn parse_object_start(&mut self) -> Option<u8> {
        self.parse_byte(b'{')
    }
    pub fn parse_object_end(&mut self) -> Option<u8> {
        self.parse_byte(b'}')
    }

    pub fn parse_array_start(&mut self) -> Option<u8> {
        self.parse_byte(b'[')
    }
    pub fn parse_array_end(&mut self) -> Option<u8> {
        self.parse_byte(b']')
    }

    // TODO: these should probably just return bool
    pub fn try_parse_object_start(&mut self) -> Option<u8> {
        self.try_parse_byte(b'{')
    }
    pub fn try_parse_object_end(&mut self) -> Option<u8> {
        self.try_parse_byte(b'}')
    }

    pub fn try_parse_array_start(&mut self) -> Option<u8> {
        self.try_parse_byte(b'[')
    }
    pub fn try_parse_array_end(&mut self) -> Option<u8> {
        self.try_parse_byte(b']')
    }

    //     pub fn try_parse_null(&mut self) -> Option<()> {
    //         // TODO
    //         if let Some(b'n') = self.null() => {
    //             let _ = parser.next();
    //             let _ = parser.next();
    //             let _ = parser.next();
    //             let _ = parser.next();
    //             Some(())
    //         } else {
    //             None
    //         }
    //     }

    pub fn parse_name(&mut self) -> Option<&'de str> {
        self.parse_byte(b'\"')?; // TODO: this can advance inside string
        let s = self.parse_str()?;
        self.parse_byte(b'\"')?;
        let _ = self.parse_byte(b':');
        Some(s)
    }

    pub fn try_parse_name(&mut self) -> Option<&'de str> {
        self.peek_byte(b'\"')?;
        let _ = self.next();
        let s = self.parse_str()?;
        self.parse_byte(b'\"')?;
        self.parse_byte(b':')?;
        Some(s)
    }

    pub fn parse_deserialize<T>(&mut self) -> Option<T>
    where
        T: Deserialize<'de>,
    {
        let value = T::deserialize(self);
        self.advance();
        value
    }

    pub fn parse_element(&mut self) -> Option<&'de [u8]> {
        self.input.parse_element()
    }
}

#[macro_export]
macro_rules! collect_with {
    ($parser:ident, $until:literal => $fn:block) => {
        std::iter::from_fn(|| $parser.peek_byte($until).is_none().then(|| $fn))
            .collect::<Option<Self>>()
    };
}

pub struct Parser<R> {
    input: R,
}

pub trait Read<'de> {
    fn next(&mut self) -> Option<u8>;
    fn peek(&mut self) -> Option<u8>;
    fn parse_str(&mut self) -> Option<&'de str>;
    fn next_slice(&mut self) -> Option<&'de [u8]>; // hacky float
    fn parse_element(&mut self) -> Option<&'de [u8]>;
}

struct SliceRead<'a> {
    slice: &'a [u8],
}

impl<'de> Read<'de> for SliceRead<'de> {
    fn next(&mut self) -> Option<u8> {
        if self.slice.len() != 0 {
            let byte = self.slice[0];
            self.slice = &self.slice[1..];
            Some(byte)
        } else {
            None
        }
    }

    fn peek(&mut self) -> Option<u8> {
        if self.slice.len() != 0 {
            Some(self.slice[0])
        } else {
            None
        }
    }

    fn parse_str(&mut self) -> Option<&'de str> {
        let idx = memchr::memchr_iter(b'"', self.slice).find(|idx| self.slice[idx - 1] != b'\\')?;
        let str = std::str::from_utf8(&self.slice[..idx]).ok()?;
        self.slice = &self.slice[idx..];
        Some(str)
    }

    fn next_slice(&mut self) -> Option<&'de [u8]> {
        let idx = memchr::memchr3(b' ', b',', b']', self.slice)?;
        let ret = &self.slice[..idx];
        self.slice = &self.slice[idx..];
        Some(ret)
    }

    fn parse_element(&mut self) -> Option<&'de [u8]> {
        let mut current = 0;
        match self.slice.get(0)? {
            b'{' => {
                let mut not_in_string = true;
                loop {
                    current += 1;
                    match self.slice.get(current) {
                        Some(b'}') if not_in_string => {
                            current += 1;
                            break;
                        }
                        Some(b'"') if self.slice.get(current - 1) != Some(&b'\\') => {
                            not_in_string = !not_in_string
                        }
                        None => return None,
                        _ => {}
                    }
                }
            }

            b'[' => {
                let mut not_in_string = true;
                loop {
                    current += 1;
                    match self.slice.get(current) {
                        Some(b']') if not_in_string => {
                            current += 1;
                            break;
                        }
                        Some(b'"') if self.slice.get(current - 1) != Some(&b'\\') => {
                            not_in_string = !not_in_string
                        }
                        None => return None,
                        _ => {}
                    }
                }
            }

            b'"' => loop {
                current += 1;
                match self.slice.get(current) {
                    Some(b'"') if self.slice.get(current - 1) != Some(&b'\\') => {
                        current += 1;
                        break;
                    }
                    None => return None,
                    _ => {}
                }
            },

            _ => loop {
                current += 1;
                match self.slice.get(current) {
                    Some(b' ' | b',' | b'\n' | b'}' | b']') | None => break,
                    _ => {}
                }
            },
        }

        let ret = &self.slice[..current];
        self.slice = &self.slice[current..];
        Some(ret)
    }
}

pub fn from_slice<'de, T: Deserialize<'de>>(slice: &'de [u8]) -> Option<T> {
    let mut parser = Parser {
        input: SliceRead { slice },
    };
    T::deserialize(&mut parser)
}

macro_rules! impl_integer {
    ($t:ty) => {
        impl<'de> Deserialize<'de> for $t {
            fn deserialize<R: Read<'de>>(parser: &mut Parser<R>) -> Option<Self> {
                let mut val: $t = 0;
                loop {
                    match parser.peek() {
                        Some(b'0'..=b'9') => val = val * 10 + (parser.next()? - b'0') as $t,
                        _ => break,
                    }
                }
                parser.advance();
                Some(val)
            }
        }
    };
}

macro_rules! impl_float {
    ($t:ty) => {
        impl<'de> Deserialize<'de> for $t {
            fn deserialize<R: Read<'de>>(parser: &mut Parser<R>) -> Option<Self> {
                let slice = parser.input.next_slice()?;
                let str = std::str::from_utf8(slice).ok()?;
                let float = str.parse::<$t>().ok()?;
                parser.advance();
                Some(float)
            }
        }
    };
}

impl_integer!(u8);
impl_integer!(i8);
impl_integer!(u16);
impl_integer!(i16);
impl_integer!(u32);
impl_integer!(i32);
impl_integer!(u64);
impl_integer!(i64);
impl_integer!(usize);
impl_integer!(isize);
impl_float!(f32);
impl_float!(f64);

impl<'de> Deserialize<'de> for bool {
    fn deserialize<R: Read<'de>>(parser: &mut Parser<R>) -> Option<Self> {
        // TODO not ideal
        let element = match parser.next()? {
            b't' => true,
            b'f' => {
                let _ = parser.next();
                false
            }
            _ => return None,
        };
        let _ = parser.next();
        let _ = parser.next();
        let _ = parser.next();
        parser.advance();
        Some(element)
    }
}

impl<'de, T> Deserialize<'de> for Option<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<R: Read<'de>>(parser: &mut Parser<R>) -> Option<Self> {
        // TODO also not ideal
        let element = match parser.peek()? {
            b'n' => {
                let _ = parser.next();
                let _ = parser.next();
                let _ = parser.next();
                let _ = parser.next();
                None
            }
            _ => parser.parse_deserialize::<T>(),
        };
        parser.advance();
        Some(element)
    }
}

impl<'de, T> Deserialize<'de> for Box<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<R: Read<'de>>(parser: &mut Parser<R>) -> Option<Self> {
        let element = Box::new(parser.parse_deserialize::<T>()?);
        parser.advance();
        Some(element)
    }
}

impl<'de, T> Deserialize<'de> for Vec<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<R: Read<'de>>(parser: &mut Parser<R>) -> Option<Self> {
        parser.parse_byte(b'[')?;
        let element = collect_with!(parser, b']' => { parser.parse_deserialize::<T>() })?;
        parser.parse_byte(b']')?;
        Some(element)
    }
}

impl<'de> Deserialize<'de> for String {
    fn deserialize<R: Read<'de>>(parser: &mut Parser<R>) -> Option<Self> {
        let value = match parser.next()? {
            b'"' => parser.input.parse_str(),
            _ => None,
        }?
        .to_string();
        match parser.next()? {
            b'"' => {}
            _ => return None,
        }
        parser.advance();
        Some(value)
    }
}

impl<'de> Deserialize<'de> for &'de str {
    fn deserialize<R: Read<'de>>(parser: &mut Parser<R>) -> Option<Self> {
        let value = match parser.next()? {
            b'"' => parser.input.parse_str(),
            _ => None,
        }?;
        match parser.next()? {
            b'"' => {}
            _ => return None,
        }
        parser.advance();
        Some(value)
    }
}

impl<'de, T> Deserialize<'de> for Cow<'static, T>
where
    T: ToOwned + ?Sized,
    <T as ToOwned>::Owned: Deserialize<'de>,
{
    fn deserialize<R: Read<'de>>(parser: &mut Parser<R>) -> Option<Self> {
        <<T as ToOwned>::Owned>::deserialize(parser).map(Cow::Owned)
    }
}

impl<'de, K, V> Deserialize<'de> for HashMap<K, V>
where
    K: Deserialize<'de> + Hash + Eq,
    V: Deserialize<'de>,
{
    fn deserialize<R: Read<'de>>(parser: &mut Parser<R>) -> Option<Self> {
        parser.parse_byte(b'{')?;
        let map = collect_with!(parser, b'}' => {
            let key = K::deserialize(parser)?;
            parser.advance();
            parser.parse_byte(b':')?;
            let value = V::deserialize(parser)?;
            parser.advance();
            Some((key, value))
        })?;
        parser.parse_byte(b'}')?;
        Some(map)
    }
}

impl<'de> Deserialize<'de> for () {
    fn deserialize<R: Read<'de>>(parser: &mut Parser<R>) -> Option<Self> {
        parser.parse_byte(b'n')?;
        parser.parse_byte(b'u')?;
        parser.parse_byte(b'l')?;
        parser.parse_byte(b'l')?;
        parser.advance();
        Some(())
    }
}

impl<'de, T, const N: usize> Deserialize<'de> for [T; N]
where
    T: Deserialize<'de>,
{
    fn deserialize<R: Read<'de>>(parser: &mut Parser<R>) -> Option<Self> {
        parser.parse_byte(b'[')?;
        let mut array: core::mem::MaybeUninit<[T; N]> = core::mem::MaybeUninit::uninit();
        let ptr = array.as_mut_ptr() as *mut T;
        for (i, maybe_element) in (0..N).map(|i| (i, parser.parse_deserialize::<T>())) {
            match maybe_element {
                Some(element) => unsafe { *ptr.add(i) = element },
                None => return None,
            }
        }
        parser.parse_byte(b']')?;
        Some(unsafe { array.assume_init() })
    }
}

impl<'de> Deserialize<'de> for Value<'de> {
    fn deserialize<R: Read<'de>>(parser: &mut Parser<R>) -> Option<Self> {
        let element = parser.parse_element()?;
        parser.advance();
        Some(Self(element))
    }
}

pub trait DeserializePartial<'de, R: Read<'de>> {
    type Partial: Partial<'de, R>;
}

pub trait Partial<'de, R: Read<'de>> {
    // TODO: this should probably be a generic parameter
    type Full;
    fn new() -> Self;
    fn try_parse(&mut self, field: &'de str, parser: &mut Parser<R>) -> Option<()>;
    fn build(self) -> Option<Self::Full>;
}

// special case: partial for hashmaps
impl<'de, R, V> DeserializePartial<'de, R> for HashMap<String, V>
where
    R: Read<'de>,
    V: Deserialize<'de>,
{
    type Partial = HashMapPartial<V>;
}

pub struct HashMapPartial<V> {
    elements: HashMap<String, V>,
}

impl<'de, R, V> Partial<'de, R> for HashMapPartial<V>
where
    R: Read<'de>,
    V: Deserialize<'de>,
{
    type Full = HashMap<String, V>;

    fn new() -> Self {
        Self {
            elements: HashMap::new(),
        }
    }

    fn try_parse(&mut self, field: &'de str, parser: &mut Parser<R>) -> Option<()> {
        self.elements
            .insert(field.to_string(), parser.parse_deserialize::<V>()?);
        Some(())
    }

    fn build(self) -> Option<Self::Full> {
        let Self { elements } = self;
        Some(elements)
    }
}
