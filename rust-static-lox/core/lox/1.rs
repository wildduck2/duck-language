#!/usr/bin/env rust-script
#![allow(dead_code, unused_imports, unused_variables)]
#![cfg_attr(not(test), allow(unused_macros))]

/* ------------------------------------------------------------------------- */
/* Imports + modules                                                         */
/* ------------------------------------------------------------------------- */

use core::marker::PhantomData;
use core::ops::{Add, Deref, DerefMut};
use std::collections::{BTreeMap, HashMap};

pub mod inner {
  #![allow(clippy::all)]
  pub mod nested {
    pub const ANSWER: i32 = 42;
    pub static mut GLOBAL: i32 = 0;
  }
}

/* ------------------------------------------------------------------------- */
/* Macros                                                                    */
/* ------------------------------------------------------------------------- */

macro_rules! add {
  ($a:expr, $b:expr) => {
    $a + $b
  };
}

macro_rules! log_kv {
    // repetition + separator
    ($( $k:ident = $v:expr ),* $(,)?) => {{
        let mut m = ::std::collections::BTreeMap::new();
        $( m.insert(stringify!($k), $v); )*
        m
    }};
}

macro_rules! nested_delims {
    // nested delimited TT
    ($($tt:tt)*) => {{
        let _x = ( { [ $($tt)* ] } );
        _x
    }};
}

macro_rules! make_vec {
    ($($x:expr),* $(,)?) => {
        vec![$($x),*]
    };
}

/* ------------------------------------------------------------------------- */
/* Foreign items                                                             */
/* ------------------------------------------------------------------------- */

extern "C" {
  fn puts(s: *const i8) -> i32;
  static mut errno: i32;
}

extern "C" {
  type OpaqueC;
}

/* ------------------------------------------------------------------------- */
/* Types                                                                     */
/* ------------------------------------------------------------------------- */

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Error {
  pub msg: String,
}

#[repr(C, packed)]
pub struct ReprPacked {
  a: u8,
  b: u16,
}

pub union U {
  i: u32,
  f: f32,
}

pub enum E<'a, T, const N: usize>
where
  T: Copy + 'a,
{
  Unit,
  Tuple(u8, &'a T),
  Array([T; N]),
  Struct { x: T, y: T },
}

pub struct Wrap<T>(pub T);

impl<T> Deref for Wrap<T> {
  type Target = T;
  fn deref(&self) -> &T {
    &self.0
  }
}

impl<T> DerefMut for Wrap<T> {
  fn deref_mut(&mut self) -> &mut T {
    &mut self.0
  }
}

/* ------------------------------------------------------------------------- */
/* Traits                                                                    */
/* ------------------------------------------------------------------------- */

pub trait TraitA {
  type Assoc<'a>: Copy
  where
    Self: 'a;

  const C: usize;

  fn f(&self) -> i32;

  fn g<T>(&self, x: T) -> T
  where
    T: Copy,
  {
    x
  }
}

pub unsafe trait UnsafeTrait {
  fn u(&self) -> usize;
}

pub trait IteratorLike {
  type Item;
  fn next(&mut self) -> Option<Self::Item>;
}

pub trait IntoIterLike {
  type Item;
  type IntoIter: IteratorLike<Item = Self::Item>;
  fn into_iter_like(self) -> Self::IntoIter;
}

/* ------------------------------------------------------------------------- */
/* Impl blocks                                                               */
/* ------------------------------------------------------------------------- */

pub struct S<const N: usize, T> {
  pub buf: [T; N],
}

impl<const N: usize, T: Copy> S<N, T> {
  pub fn new(fill: T) -> Self {
    Self { buf: [fill; N] }
  }
}

unsafe impl UnsafeTrait for ReprPacked {
  fn u(&self) -> usize {
    1
  }
}

impl TraitA for ReprPacked {
  type Assoc<'a>
    = u8
  where
    Self: 'a;

  const C: usize = 3;

  fn f(&self) -> i32 {
    add!(1, 2)
  }
}

/* ------------------------------------------------------------------------- */
/* Functions                                                                 */
/* ------------------------------------------------------------------------- */

pub fn takes_impl(x: impl core::fmt::Debug) -> usize {
  let _ = format!("{:?}", x);
  0
}

pub fn returns_impl() -> impl core::fmt::Debug {
  123u32
}

pub fn takes_dyn(x: &dyn core::fmt::Debug) -> usize {
  let _ = format!("{:?}", x);
  0
}

pub fn returns_dyn_boxed() -> Box<dyn core::fmt::Debug> {
  Box::new("hello")
}

pub fn generic_fn<'a, T, const N: usize>(x: &'a [T; N]) -> &'a T
where
  T: Copy + 'a,
{
  &x[0]
}

/* ------------------------------------------------------------------------- */
/* Patterns + match + control flow                                           */
/* ------------------------------------------------------------------------- */

pub fn pattern_party<'a, T: Copy>(e: E<'a, T, 3>) -> i32 {
  match e {
    E::Unit => 0,
    E::Tuple(n, t) if n > 0 => 1,
    E::Tuple(_, _) => 2,
    E::Array([a, b, c]) => 3,
    E::Struct { x, y } => {
      let _ = (x, y);
      4
    },
  }
}
