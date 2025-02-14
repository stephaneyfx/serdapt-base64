// Copyright (c) 2024 Stephane Raux. Distributed under the 0BSD license.

//! # Overview
//! - [📦 crates.io](https://crates.io/crates/serdapt-base64)
//! - [📖 Documentation](https://docs.rs/serdapt-base64)
//! - [⚖ 0BSD license](https://spdx.org/licenses/0BSD.html)
//!
//! Base64 adapter for `#[serde(with = ...)]`. See [`serdapt`](https://docs.rs/serdapt) for more
//! information on how to use such adapters.
//!
//! The documentation for [`Base64`] and [`Base64Array`] provides examples.
//!
//! # Contribute
//! All contributions shall be licensed under the [0BSD license](https://spdx.org/licenses/0BSD.html).

#![deny(missing_docs)]
#![no_std]

extern crate alloc;

use alloc::vec::Vec;
use base64::Engine;
use core::{
    fmt::{self, Display},
    marker::PhantomData,
};
use serdapt::{DeserializeWith, SerializeWith};
use serde::{de::Visitor, Deserializer, Serialize, Serializer};

/// Adapter to serialize bytes as a base64 string
///
/// If the target type is an array, the [`Base64Array`] adapter should perform better.
///
/// # Example
/// ```
/// # extern crate alloc;
/// # use alloc::{vec, vec::Vec};
/// use serde::{Deserialize, Serialize};
/// use serde_json::json;
///
/// #[derive(Debug, Deserialize, PartialEq, Serialize)]
/// struct Foo(#[serde(with = "serdapt_base64::StdBase64")] Vec<u8>);
///
/// let x = Foo(vec![9, 1, 67]);
/// let v = serde_json::to_value(&x).unwrap();
/// assert_eq!(v, json!("CQFD"));
/// let x2 = serde_json::from_value::<Foo>(v).unwrap();
/// assert_eq!(x, x2);
/// ```
pub struct Base64<A = Standard> {
    alphabet: PhantomData<A>,
}

/// Adapter to serialize bytes as a base64 string using the standard alphabet
pub type StdBase64 = Base64;

impl<A> Base64<A> {
    /// Serializes value with adapter
    pub fn serialize<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
    where
        T: ?Sized,
        S: Serializer,
        Self: SerializeWith<T>,
    {
        Self::serialize_with(value, serializer)
    }

    /// Deserializes value with adapter
    pub fn deserialize<'de, T, D>(deserializer: D) -> Result<T, D::Error>
    where
        D: Deserializer<'de>,
        Self: DeserializeWith<'de, T>,
    {
        Self::deserialize_with(deserializer)
    }
}

impl<A, T> SerializeWith<T> for Base64<A>
where
    A: AlphabetTag,
    T: AsRef<[u8]>,
{
    fn serialize_with<S: Serializer>(bytes: &T, serializer: S) -> Result<S::Ok, S::Error> {
        Serialize::serialize(&A::VALUE.inner.encode(bytes), serializer)
    }
}

impl<'de, A, T> DeserializeWith<'de, T> for Base64<A>
where
    A: AlphabetTag,
    T: TryFrom<Vec<u8>>,
    T::Error: Display,
{
    fn deserialize_with<D>(deserializer: D) -> Result<T, D::Error>
    where
        D: Deserializer<'de>,
    {
        let bytes = deserializer.deserialize_str(Base64Visitor::new::<A>())?;
        bytes.try_into().map_err(serde::de::Error::custom)
    }
}

struct Base64Visitor {
    engine: &'static base64::engine::GeneralPurpose,
}

impl Base64Visitor {
    const fn new<A>() -> Self
    where
        A: AlphabetTag,
    {
        Self {
            engine: &A::VALUE.inner,
        }
    }
}

impl Visitor<'_> for Base64Visitor {
    type Value = Vec<u8>;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("a base64 string")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.engine.decode(v).map_err(serde::de::Error::custom)
    }
}

/// Adapter to serialize a byte array as a base64 string
///
/// # Example
/// ```
/// # extern crate alloc;
/// # use alloc::vec;
/// use serde::{Deserialize, Serialize};
/// use serde_json::json;
///
/// #[derive(Debug, Deserialize, PartialEq, Serialize)]
/// struct Foo(#[serde(with = "serdapt_base64::StdBase64Array")] [u8; 3]);
///
/// let x = Foo([9, 1, 67]);
/// let v = serde_json::to_value(&x).unwrap();
/// assert_eq!(v, json!("CQFD"));
/// let x2 = serde_json::from_value::<Foo>(v).unwrap();
/// assert_eq!(x, x2);
/// ```
pub struct Base64Array<A = Standard> {
    alphabet: PhantomData<A>,
}

/// Adapter to serialize a byte array as a base64 string using the standard alphabet
pub type StdBase64Array = Base64Array;

impl<A> Base64Array<A> {
    /// Serializes value with adapter
    pub fn serialize<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
    where
        T: ?Sized,
        S: Serializer,
        Self: SerializeWith<T>,
    {
        Self::serialize_with(value, serializer)
    }

    /// Deserializes value with adapter
    pub fn deserialize<'de, T, D>(deserializer: D) -> Result<T, D::Error>
    where
        D: Deserializer<'de>,
        Self: DeserializeWith<'de, T>,
    {
        Self::deserialize_with(deserializer)
    }
}

impl<A, T> SerializeWith<T> for Base64Array<A>
where
    A: AlphabetTag,
    T: AsRef<[u8]>,
{
    fn serialize_with<S: Serializer>(bytes: &T, serializer: S) -> Result<S::Ok, S::Error> {
        Serialize::serialize(&A::VALUE.inner.encode(bytes), serializer)
    }
}

impl<'de, A, const N: usize> DeserializeWith<'de, [u8; N]> for Base64Array<A>
where
    A: AlphabetTag,
{
    fn deserialize_with<D>(deserializer: D) -> Result<[u8; N], D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(Base64ArrayVisitor::<N>::new::<A>())
    }
}

struct Base64ArrayVisitor<const N: usize> {
    engine: &'static base64::engine::GeneralPurpose,
}

impl<const N: usize> Base64ArrayVisitor<N> {
    const fn new<A>() -> Self
    where
        A: AlphabetTag,
    {
        Self {
            engine: &A::VALUE.inner,
        }
    }
}

impl<const N: usize> Visitor<'_> for Base64ArrayVisitor<N> {
    type Value = [u8; N];

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "a base64 string encoding {N} bytes")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        let mut out = [0u8; N];
        let n = self.engine.decode_slice(v, &mut out).map_err(|e| match e {
            base64::DecodeSliceError::DecodeError(_) => serde::de::Error::custom(e),
            base64::DecodeSliceError::OutputSliceTooSmall => {
                serde::de::Error::invalid_length(N + 1, &self)
            }
        })?;
        Some(out)
            .filter(|_| n == N)
            .ok_or_else(|| serde::de::Error::invalid_length(n, &self))
    }
}

/// Types representing a base64 alphabet
pub trait AlphabetTag {
    /// Alphabet instance
    const VALUE: Alphabet;
}

/// Alphabet definition
#[derive(Debug)]
pub struct Alphabet {
    inner: base64::engine::GeneralPurpose,
}

impl Alphabet {
    /// Constructs a base64 alphabet using the provided characters
    ///
    /// Panics if the character set is invalid (e.g. if it contains '=').
    pub const fn new(chars: &[u8; 64], padding: PaddingStrategy) -> Alphabet {
        Self {
            inner: engine(chars, padding),
        }
    }
}

/// Adapter argument to use the standard base64 alphabet with required padding
#[derive(Debug)]
pub struct Standard;

impl AlphabetTag for Standard {
    const VALUE: Alphabet = Alphabet {
        inner: base64::engine::general_purpose::STANDARD,
    };
}

/// Adapter argument to use the URL-safe base64 alphabet with required padding
#[derive(Debug)]
pub struct UrlSafe;

impl AlphabetTag for UrlSafe {
    const VALUE: Alphabet = Alphabet {
        inner: base64::engine::general_purpose::URL_SAFE,
    };
}

const fn engine(chars: &[u8; 64], padding: PaddingStrategy) -> base64::engine::GeneralPurpose {
    let s = match core::str::from_utf8(chars) {
        Ok(s) => s,
        Err(_) => panic!("Invalid base64 alphabet"),
    };
    let alphabet = match base64::alphabet::Alphabet::new(s) {
        Ok(a) => a,
        Err(_) => panic!("Invalid base64 alphabet"),
    };
    base64::engine::GeneralPurpose::new(
        &alphabet,
        base64::engine::GeneralPurposeConfig::new()
            .with_encode_padding(padding.write())
            .with_decode_padding_mode(padding.padding_mode()),
    )
}

/// Ways to handle padding
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum PaddingStrategy {
    /// Padding is not written and is ignored if read
    Ignored,
    /// Padding is required
    Required,
    /// Padding is disallowed
    None,
}

impl PaddingStrategy {
    const fn padding_mode(self) -> base64::engine::DecodePaddingMode {
        match self {
            Self::Ignored => base64::engine::DecodePaddingMode::Indifferent,
            Self::Required => base64::engine::DecodePaddingMode::RequireCanonical,
            Self::None => base64::engine::DecodePaddingMode::RequireNone,
        }
    }

    const fn write(self) -> bool {
        match self {
            Self::Required => true,
            Self::Ignored | Self::None => false,
        }
    }
}
