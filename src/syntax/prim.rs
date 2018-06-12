//! Primitive operations

use nameless::{Name, Var};
use std::fmt;
use std::rc::Rc;

use syntax::core::{Literal, Type, Value};

// Some helper traits for marshalling between Rust and Pikelet values
//
// I'm not super happy with the API at the moment, so these are currently private

trait IntoValue {
    fn into_value(self) -> Rc<Value>;
}

trait TryFromValueRef {
    fn try_from_value_ref(src: &Value) -> Result<&Self, ()>;
}

trait HasType {
    fn ty() -> Rc<Type>;
}

macro_rules! impl_into_value {
    ($T:ty, $Variant:ident) => {
        impl IntoValue for $T {
            fn into_value(self) -> Rc<Value> {
                Rc::new(Value::Literal(Literal::$Variant(self)))
            }
        }
    };
}

impl_into_value!(String, String);
impl_into_value!(char, Char);
impl_into_value!(bool, Bool);
impl_into_value!(u8, U8);
impl_into_value!(u16, U16);
impl_into_value!(u32, U32);
impl_into_value!(u64, U64);
impl_into_value!(i8, I8);
impl_into_value!(i16, I16);
impl_into_value!(i32, I32);
impl_into_value!(i64, I64);
impl_into_value!(f32, F32);
impl_into_value!(f64, F64);

macro_rules! impl_try_from_value_ref {
    ($T:ty, $Variant:ident) => {
        impl TryFromValueRef for $T {
            fn try_from_value_ref(src: &Value) -> Result<&Self, ()> {
                match *src {
                    Value::Literal(Literal::$Variant(ref x)) => Ok(x),
                    _ => Err(()),
                }
            }
        }
    };
}

impl_try_from_value_ref!(String, String);
impl_try_from_value_ref!(char, Char);
impl_try_from_value_ref!(bool, Bool);
impl_try_from_value_ref!(u8, U8);
impl_try_from_value_ref!(u16, U16);
impl_try_from_value_ref!(u32, U32);
impl_try_from_value_ref!(u64, U64);
impl_try_from_value_ref!(i8, I8);
impl_try_from_value_ref!(i16, I16);
impl_try_from_value_ref!(i32, I32);
impl_try_from_value_ref!(i64, I64);
impl_try_from_value_ref!(f32, F32);
impl_try_from_value_ref!(f64, F64);

macro_rules! impl_has_ty {
    ($T:ty, $ty_name:expr) => {
        impl HasType for $T {
            fn ty() -> Rc<Type> {
                Rc::new(Value::from(Var::Free(Name::user($ty_name))))
            }
        }
    };
}

impl_has_ty!(String, "String");
impl_has_ty!(char, "Char");
impl_has_ty!(bool, "Bool");
impl_has_ty!(u8, "U8");
impl_has_ty!(u16, "U16");
impl_has_ty!(u32, "U32");
impl_has_ty!(u64, "U64");
impl_has_ty!(i8, "I8");
impl_has_ty!(i16, "I16");
impl_has_ty!(i32, "I32");
impl_has_ty!(i64, "I64");
impl_has_ty!(f32, "F32");
impl_has_ty!(f64, "F64");

// TODO: Return a `Result` with better errors
pub type NormFn = fn(&[Rc<Value>]) -> Result<Rc<Value>, ()>;

/// Primitive functions
#[derive(Clone)]
pub struct PrimFn {
    /// A name to be used when translating the primitive to the target language
    /// during compilation
    pub name: String,
    /// The number of arguments to pass to the primitive during normalization
    pub arity: usize,
    /// The type of the primitive
    pub ann: Rc<Type>,
    /// The primitive definition to be used during normalization
    pub fun: NormFn,
}

impl fmt::Debug for PrimFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("PrimFn")
            .field("name", &self.name)
            .field("arity", &self.arity)
            .field("ann", &self.ann)
            .field("fun", &"|params| { .. }")
            .finish()
    }
}

/// Boilerplate macro for counting the number of supplied token trees
macro_rules! count {
    () => (0_usize);
    ( $x:tt $($xs:tt)* ) => (1_usize + count!($($xs)*));
}

/// Define a primitive function
macro_rules! def_prim {
    ($id:ident, $name:expr,fn($($param_name:ident : $PType:ty),*) -> $RType:ty $body:block) => {
        pub fn $id() -> PrimFn {
            use nameless::{self, Embed, Name};

            fn fun(params: &[Rc<Value>]) -> Result<Rc<Value>, ()> {
                match params[..] {
                    [$(ref $param_name),*] => {
                        $(let $param_name = <$PType>::try_from_value_ref($param_name)?;)*
                        Ok(<$RType>::into_value($body))
                    },
                    _ => Err(()) // TODO: Better errors
                }
            }

            let name = $name.to_string();
            let arity = count!($($param_name)*);
            let mut ann = <$RType>::ty();
            $(ann = Rc::new(Value::Pi(nameless::bind(
                (Name::user(stringify!($param_name)), Embed(<$PType>::ty())),
                ann
            )));)+

            PrimFn { name, arity, ann, fun }
        }
    };
}

// Primitive functions
//
// These are included in the default context, which can be created by via the
// `Context::default` function.

def_prim!(string_eq, "prim-string-eq", fn(x: String, y: String) -> bool { x == y });
def_prim!(bool_eq, "prim-bool-eq", fn(x: bool, y: bool) -> bool { x == y });
def_prim!(char_eq, "prim-char-eq", fn(x: char, y: char) -> bool { x == y });
def_prim!(u8_eq, "prim-u8-eq", fn(x: u8, y: u8) -> bool { x == y });
def_prim!(u16_eq, "prim-u16-eq", fn(x: u16, y: u16) -> bool { x == y });
def_prim!(u32_eq, "prim-u32-eq", fn(x: u32, y: u32) -> bool { x == y });
def_prim!(u64_eq, "prim-u64-eq", fn(x: u64, y: u64) -> bool { x == y });
def_prim!(i8_eq, "prim-i8-eq", fn(x: i8, y: i8) -> bool { x == y });
def_prim!(i16_eq, "prim-i16-eq", fn(x: i16, y: i16) -> bool { x == y });
def_prim!(i32_eq, "prim-i32-eq", fn(x: i32, y: i32) -> bool { x == y });
def_prim!(i64_eq, "prim-i64-eq", fn(x: i64, y: i64) -> bool { x == y });
def_prim!(f32_eq, "prim-f32-eq", fn(x: f32, y: f32) -> bool { f32::eq(x, y) });
def_prim!(f64_eq, "prim-f64-eq", fn(x: f64, y: f64) -> bool { f64::eq(x, y) });

def_prim!(string_ne, "prim-string-ne", fn(x: String, y: String) -> bool { x != y });
def_prim!(bool_ne, "prim-bool-ne", fn(x: bool, y: bool) -> bool { x != y });
def_prim!(char_ne, "prim-char-ne", fn(x: char, y: char) -> bool { x != y });
def_prim!(u8_ne, "prim-u8-ne", fn(x: u8, y: u8) -> bool { x != y });
def_prim!(u16_ne, "prim-u16-ne", fn(x: u16, y: u16) -> bool { x != y });
def_prim!(u32_ne, "prim-u32-ne", fn(x: u32, y: u32) -> bool { x != y });
def_prim!(u64_ne, "prim-u64-ne", fn(x: u64, y: u64) -> bool { x != y });
def_prim!(i8_ne, "prim-i8-ne", fn(x: i8, y: i8) -> bool { x != y });
def_prim!(i16_ne, "prim-i16-ne", fn(x: i16, y: i16) -> bool { x != y });
def_prim!(i32_ne, "prim-i32-ne", fn(x: i32, y: i32) -> bool { x != y });
def_prim!(i64_ne, "prim-i64-ne", fn(x: i64, y: i64) -> bool { x != y });
def_prim!(f32_ne, "prim-f32-ne", fn(x: f32, y: f32) -> bool { f32::ne(x, y) });
def_prim!(f64_ne, "prim-f64-ne", fn(x: f64, y: f64) -> bool { f64::ne(x, y) });

def_prim!(string_le, "prim-string-le", fn(x: String, y: String) -> bool { x <= y });
def_prim!(bool_le, "prim-bool-le", fn(x: bool, y: bool) -> bool { x <= y });
def_prim!(char_le, "prim-char-le", fn(x: char, y: char) -> bool { x <= y });
def_prim!(u8_le, "prim-u8-le", fn(x: u8, y: u8) -> bool { x <= y });
def_prim!(u16_le, "prim-u16-le", fn(x: u16, y: u16) -> bool { x <= y });
def_prim!(u32_le, "prim-u32-le", fn(x: u32, y: u32) -> bool { x <= y });
def_prim!(u64_le, "prim-u64-le", fn(x: u64, y: u64) -> bool { x <= y });
def_prim!(i8_le, "prim-i8-le", fn(x: i8, y: i8) -> bool { x <= y });
def_prim!(i16_le, "prim-i16-le", fn(x: i16, y: i16) -> bool { x <= y });
def_prim!(i32_le, "prim-i32-le", fn(x: i32, y: i32) -> bool { x <= y });
def_prim!(i64_le, "prim-i64-le", fn(x: i64, y: i64) -> bool { x <= y });
def_prim!(f32_le, "prim-f32-le", fn(x: f32, y: f32) -> bool { x <= y });
def_prim!(f64_le, "prim-f64-le", fn(x: f64, y: f64) -> bool { x <= y });

def_prim!(string_lt, "prim-string-lt", fn(x: String, y: String) -> bool { x < y });
def_prim!(bool_lt, "prim-bool-lt", fn(x: bool, y: bool) -> bool { x < y });
def_prim!(char_lt, "prim-char-lt", fn(x: char, y: char) -> bool { x < y });
def_prim!(u8_lt, "prim-u8-lt", fn(x: u8, y: u8) -> bool { x < y });
def_prim!(u16_lt, "prim-u16-lt", fn(x: u16, y: u16) -> bool { x < y });
def_prim!(u32_lt, "prim-u32-lt", fn(x: u32, y: u32) -> bool { x < y });
def_prim!(u64_lt, "prim-u64-lt", fn(x: u64, y: u64) -> bool { x < y });
def_prim!(i8_lt, "prim-i8-lt", fn(x: i8, y: i8) -> bool { x < y });
def_prim!(i16_lt, "prim-i16-lt", fn(x: i16, y: i16) -> bool { x < y });
def_prim!(i32_lt, "prim-i32-lt", fn(x: i32, y: i32) -> bool { x < y });
def_prim!(i64_lt, "prim-i64-lt", fn(x: i64, y: i64) -> bool { x < y });
def_prim!(f32_lt, "prim-f32-lt", fn(x: f32, y: f32) -> bool { x < y });
def_prim!(f64_lt, "prim-f64-lt", fn(x: f64, y: f64) -> bool { x < y });

def_prim!(string_gt, "prim-string-gt", fn(x: String, y: String) -> bool { x > y });
def_prim!(bool_gt, "prim-bool-gt", fn(x: bool, y: bool) -> bool { x > y });
def_prim!(char_gt, "prim-char-gt", fn(x: char, y: char) -> bool { x > y });
def_prim!(u8_gt, "prim-u8-gt", fn(x: u8, y: u8) -> bool { x > y });
def_prim!(u16_gt, "prim-u16-gt", fn(x: u16, y: u16) -> bool { x > y });
def_prim!(u32_gt, "prim-u32-gt", fn(x: u32, y: u32) -> bool { x > y });
def_prim!(u64_gt, "prim-u64-gt", fn(x: u64, y: u64) -> bool { x > y });
def_prim!(i8_gt, "prim-i8-gt", fn(x: i8, y: i8) -> bool { x > y });
def_prim!(i16_gt, "prim-i16-gt", fn(x: i16, y: i16) -> bool { x > y });
def_prim!(i32_gt, "prim-i32-gt", fn(x: i32, y: i32) -> bool { x > y });
def_prim!(i64_gt, "prim-i64-gt", fn(x: i64, y: i64) -> bool { x > y });
def_prim!(f32_gt, "prim-f32-gt", fn(x: f32, y: f32) -> bool { x > y });
def_prim!(f64_gt, "prim-f64-gt", fn(x: f64, y: f64) -> bool { x > y });

def_prim!(string_ge, "prim-string-ge", fn(x: String, y: String) -> bool { x >= y });
def_prim!(bool_ge, "prim-bool-ge", fn(x: bool, y: bool) -> bool { x >= y });
def_prim!(char_ge, "prim-char-ge", fn(x: char, y: char) -> bool { x >= y });
def_prim!(u8_ge, "prim-u8-ge", fn(x: u8, y: u8) -> bool { x >= y });
def_prim!(u16_ge, "prim-u16-ge", fn(x: u16, y: u16) -> bool { x >= y });
def_prim!(u32_ge, "prim-u32-ge", fn(x: u32, y: u32) -> bool { x >= y });
def_prim!(u64_ge, "prim-u64-ge", fn(x: u64, y: u64) -> bool { x >= y });
def_prim!(i8_ge, "prim-i8-ge", fn(x: i8, y: i8) -> bool { x >= y });
def_prim!(i16_ge, "prim-i16-ge", fn(x: i16, y: i16) -> bool { x >= y });
def_prim!(i32_ge, "prim-i32-ge", fn(x: i32, y: i32) -> bool { x >= y });
def_prim!(i64_ge, "prim-i64-ge", fn(x: i64, y: i64) -> bool { x >= y });
def_prim!(f32_ge, "prim-f32-ge", fn(x: f32, y: f32) -> bool { x >= y });
def_prim!(f64_ge, "prim-f64-ge", fn(x: f64, y: f64) -> bool { x >= y });

def_prim!(u8_add, "prim-u8-add", fn(x: u8, y: u8) -> u8 { x + y });
def_prim!(u16_add, "prim-u16-add", fn(x: u16, y: u16) -> u16 { x + y });
def_prim!(u32_add, "prim-u32-add", fn(x: u32, y: u32) -> u32 { x + y });
def_prim!(u64_add, "prim-u64-add", fn(x: u64, y: u64) -> u64 { x + y });
def_prim!(i8_add, "prim-i8-add", fn(x: i8, y: i8) -> i8 { x + y });
def_prim!(i16_add, "prim-i16-add", fn(x: i16, y: i16) -> i16 { x + y });
def_prim!(i32_add, "prim-i32-add", fn(x: i32, y: i32) -> i32 { x + y });
def_prim!(i64_add, "prim-i64-add", fn(x: i64, y: i64) -> i64 { x + y });
def_prim!(f32_add, "prim-f32-add", fn(x: f32, y: f32) -> f32 { x + y });
def_prim!(f64_add, "prim-f64-add", fn(x: f64, y: f64) -> f64 { x + y });

def_prim!(u8_sub, "prim-u8-sub", fn(x: u8, y: u8) -> u8 { x - y });
def_prim!(u16_sub, "prim-u16-sub", fn(x: u16, y: u16) -> u16 { x - y });
def_prim!(u32_sub, "prim-u32-sub", fn(x: u32, y: u32) -> u32 { x - y });
def_prim!(u64_sub, "prim-u64-sub", fn(x: u64, y: u64) -> u64 { x - y });
def_prim!(i8_sub, "prim-i8-sub", fn(x: i8, y: i8) -> i8 { x - y });
def_prim!(i16_sub, "prim-i16-sub", fn(x: i16, y: i16) -> i16 { x - y });
def_prim!(i32_sub, "prim-i32-sub", fn(x: i32, y: i32) -> i32 { x - y });
def_prim!(i64_sub, "prim-i64-sub", fn(x: i64, y: i64) -> i64 { x - y });
def_prim!(f32_sub, "prim-f32-sub", fn(x: f32, y: f32) -> f32 { x - y });
def_prim!(f64_sub, "prim-f64-sub", fn(x: f64, y: f64) -> f64 { x - y });

def_prim!(u8_mul, "prim-u8-mul", fn(x: u8, y: u8) -> u8 { x * y });
def_prim!(u16_mul, "prim-u16-mul", fn(x: u16, y: u16) -> u16 { x * y });
def_prim!(u32_mul, "prim-u32-mul", fn(x: u32, y: u32) -> u32 { x * y });
def_prim!(u64_mul, "prim-u64-mul", fn(x: u64, y: u64) -> u64 { x * y });
def_prim!(i8_mul, "prim-i8-mul", fn(x: i8, y: i8) -> i8 { x * y });
def_prim!(i16_mul, "prim-i16-mul", fn(x: i16, y: i16) -> i16 { x * y });
def_prim!(i32_mul, "prim-i32-mul", fn(x: i32, y: i32) -> i32 { x * y });
def_prim!(i64_mul, "prim-i64-mul", fn(x: i64, y: i64) -> i64 { x * y });
def_prim!(f32_mul, "prim-f32-mul", fn(x: f32, y: f32) -> f32 { x * y });
def_prim!(f64_mul, "prim-f64-mul", fn(x: f64, y: f64) -> f64 { x * y });

def_prim!(u8_div, "prim-u8-div", fn(x: u8, y: u8) -> u8 { x / y });
def_prim!(u16_div, "prim-u16-div", fn(x: u16, y: u16) -> u16 { x / y });
def_prim!(u32_div, "prim-u32-div", fn(x: u32, y: u32) -> u32 { x / y });
def_prim!(u64_div, "prim-u64-div", fn(x: u64, y: u64) -> u64 { x / y });
def_prim!(i8_div, "prim-i8-div", fn(x: i8, y: i8) -> i8 { x / y });
def_prim!(i16_div, "prim-i16-div", fn(x: i16, y: i16) -> i16 { x / y });
def_prim!(i32_div, "prim-i32-div", fn(x: i32, y: i32) -> i32 { x / y });
def_prim!(i64_div, "prim-i64-div", fn(x: i64, y: i64) -> i64 { x / y });
def_prim!(f32_div, "prim-f32-div", fn(x: f32, y: f32) -> f32 { x / y });
def_prim!(f64_div, "prim-f64-div", fn(x: f64, y: f64) -> f64 { x / y });

def_prim!(char_to_string, "prim-char-to-string", fn(val: char) -> String { val.to_string() });
def_prim!(u8_to_string, "prim-u8-to-string", fn(val: u8) -> String { val.to_string() });
def_prim!(u16_to_string, "prim-u16-to-string", fn(val: u16) -> String { val.to_string() });
def_prim!(u32_to_string, "prim-u32-to-string", fn(val: u32) -> String { val.to_string() });
def_prim!(u64_to_string, "prim-u64-to-string", fn(val: u64) -> String { val.to_string() });
def_prim!(i8_to_string, "prim-i8-to-string", fn(val: i8) -> String { val.to_string() });
def_prim!(i16_to_string, "prim-i16-to-string", fn(val: i16) -> String { val.to_string() });
def_prim!(i32_to_string, "prim-i32-to-string", fn(val: i32) -> String { val.to_string() });
def_prim!(i64_to_string, "prim-i64-to-string", fn(val: i64) -> String { val.to_string() });
def_prim!(f32_to_string, "prim-f32-to-string", fn(val: f32) -> String { val.to_string() });
def_prim!(f64_to_string, "prim-f64-to-string", fn(val: f64) -> String { val.to_string() });

def_prim!(string_append, "prim-string-append", fn(x: String, y: String) -> String { x.clone() + y }); // FIXME: Clone
