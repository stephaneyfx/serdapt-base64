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
/// #[derive(Deserialize, Serialize)]
/// struct Foo(#[serde(with = "serdapt_base64::StdBase64")] Vec<u8>);
///
/// let v = serde_json::to_value(Foo(vec![9, 1, 67])).unwrap();
/// assert_eq!(v, json!("CQFD"));
/// ```
pub struct Base64<Alphabet = Standard, const WRITE_PADDING: bool = false, ReadPadding = NoPadding> {
    alphabet: PhantomData<Alphabet>,
    read_padding: PhantomData<ReadPadding>,
}

/// Adapter to serialize bytes as a base64 string using the standard alphabet
pub type StdBase64 = Base64;

impl<Alphabet, const WRITE_PADDING: bool, ReadPadding>
    Base64<Alphabet, WRITE_PADDING, ReadPadding>
{
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

impl<A, const W: bool, R, T> SerializeWith<T> for Base64<A, W, R>
where
    A: Alphabet,
    R: ReadPadding,
    T: AsRef<[u8]>,
{
    fn serialize_with<S: Serializer>(bytes: &T, serializer: S) -> Result<S::Ok, S::Error> {
        Serialize::serialize(&engine::<A, R>(W).encode(bytes), serializer)
    }
}

impl<'de, A, const W: bool, R, T> DeserializeWith<'de, T> for Base64<A, W, R>
where
    A: Alphabet,
    R: ReadPadding,
    T: TryFrom<Vec<u8>>,
    T::Error: Display,
{
    fn deserialize_with<D>(deserializer: D) -> Result<T, D::Error>
    where
        D: Deserializer<'de>,
    {
        let bytes = deserializer.deserialize_str(Base64Visitor::new::<A, R>(W))?;
        bytes.try_into().map_err(serde::de::Error::custom)
    }
}

struct Base64Visitor {
    engine: base64::engine::GeneralPurpose,
}

impl Base64Visitor {
    const fn new<A, R>(write_padding: bool) -> Self
    where
        A: Alphabet,
        R: ReadPadding,
    {
        Self {
            engine: engine::<A, R>(write_padding),
        }
    }
}

impl<'de> Visitor<'de> for Base64Visitor {
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
/// #[derive(Deserialize, Serialize)]
/// struct Foo(#[serde(with = "serdapt_base64::StdBase64Array")] [u8; 3]);
///
/// let v = serde_json::to_value(Foo([9, 1, 67])).unwrap();
/// assert_eq!(v, json!("CQFD"));
/// ```
pub struct Base64Array<
    Alphabet = Standard,
    const WRITE_PADDING: bool = false,
    ReadPadding = NoPadding,
> {
    alphabet: PhantomData<Alphabet>,
    read_padding: PhantomData<ReadPadding>,
}

/// Adapter to serialize a byte array as a base64 string using the standard alphabet
pub type StdBase64Array = Base64Array;

impl<Alphabet, const WRITE_PADDING: bool, ReadPadding>
    Base64Array<Alphabet, WRITE_PADDING, ReadPadding>
{
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

impl<A, const W: bool, R, T> SerializeWith<T> for Base64Array<A, W, R>
where
    A: Alphabet,
    R: ReadPadding,
    T: AsRef<[u8]>,
{
    fn serialize_with<S: Serializer>(bytes: &T, serializer: S) -> Result<S::Ok, S::Error> {
        Serialize::serialize(&engine::<A, R>(W).encode(bytes), serializer)
    }
}

impl<'de, A, const W: bool, R, const N: usize> DeserializeWith<'de, [u8; N]>
    for Base64Array<A, W, R>
where
    A: Alphabet,
    R: ReadPadding,
{
    fn deserialize_with<D>(deserializer: D) -> Result<[u8; N], D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(Base64ArrayVisitor::<N>::new::<A, R>(W))
    }
}

struct Base64ArrayVisitor<const N: usize> {
    engine: base64::engine::GeneralPurpose,
}

impl<const N: usize> Base64ArrayVisitor<N> {
    const fn new<A, R>(write_padding: bool) -> Self
    where
        A: Alphabet,
        R: ReadPadding,
    {
        Self {
            engine: engine::<A, R>(write_padding),
        }
    }
}

impl<'de, const N: usize> Visitor<'de> for Base64ArrayVisitor<N> {
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

/// Defines a base64 alphabet
pub trait Alphabet {
    /// Characters making up the alphabet
    const CHARS: [u8; 64];
}

/// Adapter argument to use the standard base64 alphabet
pub struct Standard;

impl Alphabet for Standard {
    const CHARS: [u8; 64] = *b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
}

/// Adapter argument to use the URL-safe base64 alphabet
pub struct UrlSafe;

impl Alphabet for UrlSafe {
    const CHARS: [u8; 64] = *b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
}

const fn engine<A, R>(write_padding: bool) -> base64::engine::GeneralPurpose
where
    A: Alphabet,
    R: ReadPadding,
{
    let s = match core::str::from_utf8(&A::CHARS) {
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
            .with_encode_padding(write_padding)
            .with_decode_padding_mode(R::STRATEGY.padding_mode()),
    )
}

/// Defines how to handle padding when decoding
pub trait ReadPadding {
    /// Strategy to handle padding when decoding
    const STRATEGY: ReadPaddingStrategy;
}

/// Adapter argument to ignore padding when decoding
pub struct Ignore;

impl ReadPadding for Ignore {
    const STRATEGY: ReadPaddingStrategy = ReadPaddingStrategy::Ignore;
}

/// Adapter argument to require canonical padding when decoding
pub struct Canonical;

impl ReadPadding for Canonical {
    const STRATEGY: ReadPaddingStrategy = ReadPaddingStrategy::Canonical;
}

/// Adapter argument to require no padding when decoding
pub struct NoPadding;

impl ReadPadding for NoPadding {
    const STRATEGY: ReadPaddingStrategy = ReadPaddingStrategy::NoPadding;
}

/// Ways to handle padding when decoding
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ReadPaddingStrategy {
    /// Require no padding
    NoPadding,
    /// Require canonical padding
    Canonical,
    /// Ignore padding
    Ignore,
}

impl ReadPaddingStrategy {
    const fn padding_mode(self) -> base64::engine::DecodePaddingMode {
        match self {
            Self::NoPadding => base64::engine::DecodePaddingMode::RequireNone,
            Self::Canonical => base64::engine::DecodePaddingMode::RequireCanonical,
            Self::Ignore => base64::engine::DecodePaddingMode::Indifferent,
        }
    }
}
