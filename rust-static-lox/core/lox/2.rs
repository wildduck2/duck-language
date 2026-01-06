#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    clippy::all,
    non_camel_case_types
)]
#![cfg_attr(not(test), allow(unused_macros))]

use core::fmt::Debug;
use core::marker::PhantomData;
use core::mem::{align_of, size_of};
use core::ops::{Add, Mul, RangeInclusive};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};

/* ------------------------------------------------------------------------- */
/* Macro_rules stress                                                        */
/* ------------------------------------------------------------------------- */

macro_rules! tt_count {
    () => { 0usize };
    ($h:tt $($t:tt)*) => { 1usize + tt_count!($($t)*) };
}

macro_rules! map_lit {
    ($( $k:expr => $v:expr ),* $(,)?) => {{
        let mut m = ::std::collections::HashMap::new();
        $( m.insert($k, $v); )*
        m
    }};
}

macro_rules! nested_tt {
    ($($tt:tt)*) => {{
        let _ = ({ [ ( $($tt)* ) ] });
        _
    }};
}

macro_rules! with_cfg {
    ($(#[$m:meta])* $item:item) => {
        $(#[$m])*
        $item
    };
}

/* ------------------------------------------------------------------------- */
/* Foreign + extern type                                                     */
/* ------------------------------------------------------------------------- */

extern "C" {
    fn strlen(s: *const i8) -> usize;
    static mut errno: i32;
}

extern "C" {
    type Opaque;
}

/* ------------------------------------------------------------------------- */
/* Modules + visibility                                                      */
/* ------------------------------------------------------------------------- */

pub(crate) mod m {
    #![allow(dead_code)]
    pub(super) fn f() -> i32 { 1 }

    pub mod deep {
        pub const X: usize = 3;
        pub static mut Y: i32 = 0;
    }
}

/* ------------------------------------------------------------------------- */
/* Types: structs, enums, unions, type aliases                               */
/* ------------------------------------------------------------------------- */

pub type Res<T> = std::result::Result<T, Err>;

#[derive(Debug, Clone)]
pub struct Err {
    pub msg: String,
}

#[repr(C)]
pub struct CRepr {
    pub a: u8,
    pub b: u32,
}

#[repr(transparent)]
pub struct Newtype<T>(pub T);

pub union Bits {
    pub u: u32,
    pub f: f32,
}

pub enum Kind {
    A = 1,
    B = 2,
    C = 3,
}

pub enum Node<'a, T, const N: usize>
where
    T: Copy + 'a,
{
    Unit,
    Leaf(T),
    Pair(&'a T, &'a T),
    Arr([T; N]),
    Rec { left: Box<Node<'a, T, N>>, right: Box<Node<'a, T, N>> },
}

/* ------------------------------------------------------------------------- */
/* Traits: GATs, associated consts, dyn, impl Trait                           */
/* ------------------------------------------------------------------------- */

pub trait Buf {
    type Item;
    fn push(&mut self, x: Self::Item);
    fn pop(&mut self) -> Option<Self::Item>;
}

pub trait HasGat {
    type Ref<'a>
    where
        Self: 'a;

    fn get<'a>(&'a self) -> Self::Ref<'a>;
}

pub trait Compute {
    const ID: u32;
    fn eval(&self, x: i32) -> i32;

    fn eval_twice(&self, x: i32) -> i32 {
        self.eval(self.eval(x))
    }
}

pub trait ObjSafe {
    fn name(&self) -> &str;
}

impl ObjSafe for Err {
    fn name(&self) -> &str { "Err" }
}

/* ------------------------------------------------------------------------- */
/* Impl blocks + where stress                                                */
/* ------------------------------------------------------------------------- */

pub struct Ring<T, const N: usize> {
    buf: [Option<T>; N],
    head: usize,
    tail: usize,
    _p: PhantomData<T>,
}

impl<T, const N: usize> Ring<T, N> {
    pub const fn new() -> Self {
        Self {
            buf: [const { None }; N],
            head: 0,
            tail: 0,
            _p: PhantomData,
        }
    }
}

impl<T: Copy, const N: usize> Buf for Ring<T, N> {
    type Item = T;

    fn push(&mut self, x: T) {
        self.buf[self.tail % N] = Some(x);
        self.tail = self.tail.wrapping_add(1);
    }

    fn pop(&mut self) -> Option<T> {
        let out = self.buf[self.head % N].take();
        self.head = self.head.wrapping_add(1);
        out
    }
}

impl HasGat for CRepr {
    type Ref<'a> = &'a u8 where Self: 'a;

    fn get<'a>(&'a self) -> Self::Ref<'a> {
        &self.a
    }
}

impl Compute for CRepr {
    const ID: u32 = 7;
    fn eval(&self, x: i32) -> i32 { x + self.a as i32 }
}

/* ------------------------------------------------------------------------- */
/* Functions: impl Trait, dyn, UFCS, qpath-ish usage                          */
/* ------------------------------------------------------------------------- */

pub fn takes_dyn(d: &dyn Debug) -> usize {
    let _ = format!("{:?}", d);
    0
}

pub fn returns_impl() -> impl Debug {
    (1u8, 2u16, 3u32)
}

pub fn returns_boxed_dyn() -> Box<dyn ObjSafe> {
    Box::new(Err { msg: "x".into() })
}

pub fn ufcs_demo<T: Compute>(x: &T, n: i32) -> i32 {
    <T as Compute>::eval_twice(x, n) + (T::ID as i32)
}

/* ------------------------------------------------------------------------- */
/* Expressions: operators, ranges, casts, indexing, calls, methods           */
/* ------------------------------------------------------------------------- */

pub fn expr_party(mut x: i32, y: i32) -> i32 {
    let a = (x + y * 2) << 1;
    let b = (a & 0b1111) ^ 0b0101 | 0b0011;

    let r: RangeInclusive<i32> = 1..=3;
    let c = if r.contains(&b) { 10 } else { 20 };

    x += c;
    x
}

/* ------------------------------------------------------------------------- */
/* Patterns: let-else, if let, match guards, destructuring                   */
/* ------------------------------------------------------------------------- */

pub fn pattern_party<'a>(n: Node<'a, i32, 3>) -> i32 {
    let Node::Arr([a, b, c]) = n else {
        return -1;
    };

    match (a, b, c) {
        (x, y, z) if x == y && y == z => 1,
        (x, y, _) if x > y => 2,
        _ => 3,
    }
}

/* ------------------------------------------------------------------------- */
/* Async + closures + await + try ?                                          */
/* ------------------------------------------------------------------------- */

async fn async_add(x: i32, y: i32) -> i32 {
    let c0 = || 1;
    let c1 = |v: i32| -> i32 { v + c0() };
    let c2 = async move |z: i32| -> i32 { z + x };

    let t = c1(y);
    c2(t).await
}

pub async fn async_result_flow() -> Res<i32> {
    let v = async_add(10, 20).await;
    let ok: Res<i32> = Ok(v);
    Ok(ok? + 1)
}

/* ------------------------------------------------------------------------- */
/* Unsafe: raw pointers, extern calls, union, static mut                      */
/* ------------------------------------------------------------------------- */

pub unsafe fn unsafe_party(s: *const i8) -> usize {
    let len = strlen(s);

    let bits = Bits { u: 0x3f800000 };
    let f = bits.f;

    errno = errno.wrapping_add(1);

    len + (f as usize)
}

/* ------------------------------------------------------------------------- */
/* Attributes meta tests via macro input                                     */
/* ------------------------------------------------------------------------- */

with_cfg! {
    #[cfg(any(unix, windows))]
    #[deprecated(since = "1.70", note = "demo")]
    pub fn attr_target() -> i32 { 7 }
};

/* ------------------------------------------------------------------------- */
/* Entry-ish                                                                  */
/* ------------------------------------------------------------------------- */

pub fn main_like() -> Res<()> {
    let _ttn = tt_count!(a b (c d) [e] {f});

    let m = map_lit!(
        "a" => 1,
        "b" => 2,
        "c" => 3,
    );

    let _nested = nested_tt!(1 + (2 * 3), { 4 }, [5]);

    let mut r: Ring<i32, 3> = Ring::new();
    r.push(10);
    r.push(20);
    r.push(30);

    let v = r.pop().ok_or_else(|| Err { msg: "empty".into() })?;

    let d = returns_impl();
    let _ = takes_dyn(&d);

    let c = CRepr { a: 1, b: 2 };
    let _u = ufcs_demo(&c, 5);

    let e = expr_party(3, 4);
    let n = Node::Arr([1, 2, 3]);
    let p = pattern_party(n);

    if v + e + p > 0 {
        Ok(())
    } else {
        Err(Err { msg: "nope".into() })
    }
}
