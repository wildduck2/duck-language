#![allow(
  dead_code,
  unused_imports,
  unused_variables,
  non_camel_case_types,
  clippy::all
)]

use core::fmt::{Debug, Display};
use core::marker::PhantomData;
use core::mem::{align_of, size_of};
use core::ops::{Add, Deref};
use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, Mutex};

/* ------------------------------------------------------------------------- */
/* Macro_rules: repetition, separators, nested delimiters, metavars          */
/* ------------------------------------------------------------------------- */

macro_rules! list {
    ($($x:expr),* $(,)?) => { vec![$($x),*] };
}

macro_rules! tuple_map {
    ($( $k:ident = $v:expr ),* $(,)?) => {{
        let mut m = ::std::collections::BTreeMap::new();
        $( m.insert(stringify!($k), $v); )*
        m
    }};
}

macro_rules! nested_matcher {
    (outer = { $($tt:tt)* }) => {{
        // force nested delims in TT
        let _x = ({ [ ( $($tt)* ) ] });
        _x
    }};
}

macro_rules! for_each {
    ($($x:ident),+ $(,)?) => {{
        $( let _ = stringify!($x); )+
    }};
}

macro_rules! accept_meta {
    ($(#[$m:meta])* $item:item) => {
        $(#[$m])*
        $item
    };
}

/* ------------------------------------------------------------------------- */
/* Modules + attrs + visibility                                              */
/* ------------------------------------------------------------------------- */

pub mod mod_a {
  #![allow(dead_code)]
  pub(crate) mod mod_b {
    pub const K: usize = 3;
    pub static mut G: i32 = 0;

    pub fn bump() {
      unsafe {
        G += 1;
      }
    }
  }
}

/* ------------------------------------------------------------------------- */
/* Types: const generics, lifetimes, unions, reprs, type aliases             */
/* ------------------------------------------------------------------------- */

pub type Res<T> = Result<T, Err>;

#[derive(Debug, Clone)]
pub struct Err {
  pub msg: String,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct CRepr {
  pub a: u8,
  pub b: u16,
}

pub union U32F32 {
  pub u: u32,
  pub f: f32,
}

pub struct ArrayBox<T, const N: usize> {
  pub buf: [T; N],
}

pub enum Msg<'a, T, const N: usize>
where
  T: Copy + 'a,
{
  Unit,
  Borrow(&'a T),
  Array([T; N]),
  Named { x: T, y: T },
}

/* ------------------------------------------------------------------------- */
/* Traits: GAT, assoc const, dyn vs impl Trait, bounds, HRTB                 */
/* ------------------------------------------------------------------------- */

pub trait Stream {
  type Item;
  fn next(&mut self) -> Option<Self::Item>;
}

pub trait IntoStream {
  type Item;
  type Into: Stream<Item = Self::Item>;
  fn into_stream(self) -> Self::Into;
}

pub trait HasGat {
  type Ref<'a>
  where
    Self: 'a;
  fn get<'a>(&'a self) -> Self::Ref<'a>;
}

pub trait Compute: Debug {
  const ID: u32;
  fn eval(&self, x: i32) -> i32;

  fn eval3(&self, x: i32) -> i32 {
    self.eval(self.eval(self.eval(x)))
  }
}

pub trait Obj: Debug + Send + Sync {
  fn name(&self) -> &str;
}

impl Obj for Err {
  fn name(&self) -> &str {
    "Err"
  }
}

/* ------------------------------------------------------------------------- */
/* Impl blocks: const generics, where, associated types, UFCS/QPath          */
/* ------------------------------------------------------------------------- */

pub struct Ring<T, const N: usize> {
  buf: [Option<T>; N],
  head: usize,
  tail: usize,
}

impl<T, const N: usize> Ring<T, N> {
  pub const fn new() -> Self {
    Self {
      buf: [const { None }; N],
      head: 0,
      tail: 0,
    }
  }

  pub fn len(&self) -> usize {
    self.tail.wrapping_sub(self.head)
  }
}

impl<T: Copy, const N: usize> Stream for Ring<T, N> {
  type Item = T;

  fn next(&mut self) -> Option<T> {
    if self.head == self.tail {
      return None;
    }
    let idx = self.head % N;
    self.head = self.head.wrapping_add(1);
    self.buf[idx].take()
  }
}

impl<T: Copy, const N: usize> Ring<T, N> {
  pub fn push(&mut self, v: T) {
    let idx = self.tail % N;
    self.buf[idx] = Some(v);
    self.tail = self.tail.wrapping_add(1);
  }
}

impl HasGat for CRepr {
  type Ref<'a>
    = &'a u8
  where
    Self: 'a;

  fn get<'a>(&'a self) -> Self::Ref<'a> {
    &self.a
  }
}

impl Compute for CRepr {
  const ID: u32 = 99;
  fn eval(&self, x: i32) -> i32 {
    x + self.a as i32
  }
}

pub fn ufcs_demo<T: Compute>(t: &T, x: i32) -> i32 {
  <T as Compute>::eval3(t, x) + (T::ID as i32)
}

/* ------------------------------------------------------------------------- */
/* impl Trait + dyn Trait usage                                              */
/* ------------------------------------------------------------------------- */

pub fn takes_impl(x: impl Debug) -> usize {
  let _ = format!("{:?}", x);
  0
}

pub fn returns_impl() -> impl Debug {
  (1u8, 2u16, 3u32)
}

pub fn takes_dyn(x: &dyn Debug) -> usize {
  let _ = format!("{:?}", x);
  0
}

pub fn returns_dyn_boxed() -> Box<dyn Obj> {
  Box::new(Err {
    msg: "boxed".into(),
  })
}

/* ------------------------------------------------------------------------- */
/* Expressions: precedence, casts, ranges, indexing, method chains           */
/* ------------------------------------------------------------------------- */

pub fn expr_stress(mut a: i32, b: i32) -> i32 {
  let x = (a + b * 2) << 1;
  let y = (x & 0b1111) ^ 0b0101 | 0b0011;
  let z = (y as i64 + 1) as i32;

  let r = 1..=3;
  if r.contains(&z) && (z != 0 || a == 0) {
    a += 10;
  } else {
    a -= 10;
  }

  a + z
}

/* ------------------------------------------------------------------------- */
/* Patterns: let-else, match, guards, destructuring                          */
/* ------------------------------------------------------------------------- */

pub fn pattern_stress<'a>(m: Msg<'a, i32, 3>) -> i32 {
  let Msg::Array([p, q, r]) = m else {
    return -1;
  };

  match (p, q, r) {
    (x, y, z) if x == y && y == z => 1,
    (x, y, _) if x > y => 2,
    _ => 3,
  }
}

/* ------------------------------------------------------------------------- */
/* Async, closures, await, ?                                                 */
/* ------------------------------------------------------------------------- */

async fn async_add(x: i32, y: i32) -> i32 {
  let c0 = || 1;
  let c1 = |v: i32| -> i32 { v + c0() };
  let c2 = async move |z: i32| -> i32 { z + x };

  let t = c1(y);
  c2(t).await
}

pub async fn async_flow() -> Res<i32> {
  let v = async_add(10, 20).await;
  let ok: Res<i32> = Ok(v);
  Ok(ok? + 1)
}

/* ------------------------------------------------------------------------- */
/* Attribute meta parsing via macro                                          */
/* ------------------------------------------------------------------------- */

accept_meta! {
    #[cfg(any(unix, windows))]
    #[deprecated(since = "1.70", note = "stress")]
    pub fn attr_target() -> i32 { 7 }
}

/* ------------------------------------------------------------------------- */
/* Const-eval-ish + generics stress                                          */
/* ------------------------------------------------------------------------- */

pub const fn pow2(n: usize) -> usize {
  1usize << n
}

pub const N3: usize = pow2(3);

pub fn const_generic_stress() -> ArrayBox<u8, 3> {
  ArrayBox { buf: [1, 2, 3] }
}

/* ------------------------------------------------------------------------- */
/* Macros usage + integration                                                */
/* ------------------------------------------------------------------------- */

pub fn macro_stress() -> (Vec<i32>, BTreeMap<&'static str, i32>) {
  let v = list![1, 2, 3, 4, 5,];
  let m = tuple_map!(a = 1, b = 2 + 3, c = 4,);

  let _ = nested_matcher!(outer = { 1 + (2 * 3), { 4 }, [5] });

  for_each!(x, y, z);

  (v, m)
}

/* ------------------------------------------------------------------------- */
/* Thread-safe stuff (parsing only)                                          */
/* ------------------------------------------------------------------------- */

pub fn sync_stress() -> Arc<Mutex<HashMap<String, i32>>> {
  let map = HashMap::<String, i32>::new();
  Arc::new(Mutex::new(map))
}

/* ------------------------------------------------------------------------- */
/* Entry-ish                                                                 */
/* ------------------------------------------------------------------------- */

pub fn main_like() -> Res<()> {
  let (_v, _m) = macro_stress();

  let mut ring: Ring<i32, 3> = Ring::new();
  ring.push(10);
  ring.push(20);
  ring.push(30);

  let got = ring.next().ok_or_else(|| Err {
    msg: "empty".into(),
  })?;

  let dbg = returns_impl();
  let _ = takes_dyn(&dbg);
  let _ = takes_impl(got);

  let c = CRepr { a: 1, b: 2 };
  let _u = ufcs_demo(&c, 5);

  let e = expr_stress(3, 4);
  let p = pattern_stress(Msg::Array([1, 2, 3]));

  let boxed = returns_dyn_boxed();
  let _name = boxed.name();

  if got + e + p > 0 {
    Ok(())
  } else {
    Err(Err { msg: "nope".into() })
  }
}
