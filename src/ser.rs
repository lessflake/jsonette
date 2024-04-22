use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::Display,
    io::{Result, Write},
};
use ryu::Buffer;
use crate::Value;

pub trait Serialize {
    fn serialize<W: Write>(&self, w: &mut W) -> Result<()>;

    #[inline]
    fn serialize_object<W: Write>(&self, w: &mut W) -> Result<()> {
        w.write_all(b"{")?;
        self.serialize(w)?;
        w.write_all(b"}")?;
        Ok(())
    }

    #[inline]
    fn serialize_array<W: Write>(&self, w: &mut W) -> Result<()> {
        w.write_all(b"[")?;
        self.serialize(w)?;
        w.write_all(b"]")?;
        Ok(())
    }

    #[inline]
    fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
        self.serialize_object(w)
    }
}

macro_rules! impl_integer {
    ($t:ty) => {
        impl Serialize for $t {
            #[inline]
            fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
                itoa::write(w, *self)?;
                Ok(())
            }
            #[inline]
            fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
                self.serialize(w)
            }
        }
    };
}

macro_rules! impl_float {
    ($t:ty) => {
        impl Serialize for $t {
            #[inline]
            fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
                w.write_all(Buffer::new().format(*self).as_bytes())
            }
            #[inline]
            fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
                self.serialize(w)
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

impl Serialize for bool {
    #[inline]
    fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
        match self {
            true => w.write_all(b"true"),
            false => w.write_all(b"false"),
        }
    }
    #[inline]
    fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
        self.serialize(w)
    }
}

impl<T> Serialize for Option<T>
where
    T: Serialize,
{
    #[inline]
    fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
        match self {
            Some(x) => x.serialize(w),
            None => w.write_all(b"null"),
        }
    }
    #[inline]
    fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
        match self {
            Some(x) => x.serialize_maybe_primitive(w),
            None => self.serialize(w),
        }
    }
}

impl<T> Serialize for Box<T>
where
    T: Serialize,
{
    #[inline]
    fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
        self.as_ref().serialize(w)
    }
    #[inline]
    fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
        self.as_ref().serialize_maybe_primitive(w)
    }
}

impl<T> Serialize for [T]
where
    T: Serialize,
{
    fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
        w.write_all(b"[")?;
        let len = self.len();
        for (i, t) in self.iter().enumerate() {
            t.serialize(w)?;
            if i < len - 1 {
                w.write_all(b",")?;
            }
        }
        w.write_all(b"]")?;
        Ok(())
    }
    #[inline]
    fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
        self.serialize(w)
    }
}

impl<T> Serialize for Vec<T>
where
    T: Serialize,
{
    fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
        <[T]>::serialize(self.as_ref(), w)
    }
    #[inline]
    fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
        self.serialize(w)
    }
}

impl Serialize for str {
    fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
        w.write_all(b"\"")?;
        w.write_all(self.as_bytes())?;
        w.write_all(b"\"")?;
        Ok(())
    }
    #[inline]
    fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
        self.serialize(w)
    }
}

impl Serialize for String {
    fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
        w.write_all(b"\"")?;
        w.write_all(self.as_bytes())?;
        w.write_all(b"\"")?;
        Ok(())
    }
    #[inline]
    fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
        self.serialize(w)
    }
}

impl<'a, T> Serialize for Cow<'a, T>
where
    T: Serialize + Clone,
{
    fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
        self.as_ref().serialize(w)
    }
    #[inline]
    fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
        self.as_ref().serialize_maybe_primitive(w)
    }
}

impl<K, V> Serialize for HashMap<K, V>
where
    K: Display,
    V: Serialize,
{
    fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
        let len = self.len();
        for (i, (k, v)) in self.iter().enumerate() {
            w.write_all(b"\"")?;
            write!(w, "{}", k)?;
            w.write_all(b"\":")?;
            v.serialize(w)?;
            if i < len - 1 {
                w.write_all(b",")?;
            }
        }
        Ok(())
    }
    #[inline]
    fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
        self.serialize_object(w)
    }
}

impl Serialize for () {
    #[inline]
    fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
        w.write_all(b"null")
    }
    #[inline]
    fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
        self.serialize(w)
    }
}

// impl<W, T> Serialize<W> for &[T]
// where
//     W: Write,
//     T: Serialize<W>,
// {
//     #[inline]
//     fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
//         let len = self.len();
//         w.write_all(b"[")?;
//         for (i, t) in self.iter().enumerate() {
//             t.serialize(w)?;
//             if i < len - 1 {
//                 w.write_all(b",")?;
//             }
//         }
//         w.write_all(b"]")?;
//         Ok(())
//     }
//     #[inline]
//     fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
//         self.serialize(w)
//     }
// }

impl<T, const N: usize> Serialize for [T; N]
where
    T: Serialize,
{
    fn serialize<W: Write>(&self, w: &mut W) -> Result<()> {
        w.write_all(b"[")?;
        for (i, t) in self.iter().enumerate() {
            t.serialize(w)?;
            if i < N - 1 {
                w.write_all(b",")?;
            }
        }
        w.write_all(b"]")?;
        Ok(())
    }
    #[inline]
    fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
        self.serialize(w)
    }
}

impl<'a> Serialize for Value<'a> {
    fn serialize<W: Write>(&self, w: &mut W) -> std::io::Result<()> {
        w.write_all(self.0)
    }
    fn serialize_maybe_primitive<W: Write>(&self, w: &mut W) -> Result<()> {
        self.serialize(w)
    }
}

