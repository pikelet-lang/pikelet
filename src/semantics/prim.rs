//! Primitive operations

use im::HashMap;
use std::fmt;

use syntax::core::{Literal, RcValue, Spine, Value};

// Some helper traits for marshalling between Rust and Pikelet values
//
// I'm not super happy with the API at the moment, so these are currently private

trait IntoValue {
    fn into_value(self) -> RcValue;
}

trait TryFromValueRef {
    fn try_from_value_ref(src: &Value) -> Result<&Self, ()>;
}

macro_rules! impl_into_value {
    ($T:ty, $Variant:ident) => {
        impl IntoValue for $T {
            fn into_value(self) -> RcValue {
                RcValue::from(Value::Literal(Literal::$Variant(self)))
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

/// Primitive functions
#[derive(Clone)]
pub struct PrimFn {
    /// The number of arguments to pass to the primitive during normalization
    pub arity: usize,
    /// The primitive definition to be used during normalization
    // TODO: Return a `Result` with better errors
    pub interpretation: fn(Spine) -> Result<RcValue, ()>,
}

impl fmt::Debug for PrimFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("PrimFn")
            .field("arity", &self.arity)
            .field("interpretation", &"|params| { .. }")
            .finish()
    }
}

#[derive(Clone, Debug)]
pub struct PrimEnv {
    definitions: HashMap<String, PrimFn>,
}

impl PrimEnv {
    pub fn get(&self, name: &str) -> Option<&PrimFn> {
        self.definitions.get(name)
    }
}

impl Default for PrimEnv {
    fn default() -> PrimEnv {
        /// Boilerplate macro for counting the number of supplied token trees
        macro_rules! count {
            () => (0_usize);
            ( $x:tt $($xs:tt)* ) => (1_usize + count!($($xs)*));
        }

        /// Define a primitive function
        macro_rules! prim {
            (fn($($param_name:ident : $PType:ty),*) -> $RType:ty $body:block) => {{
                fn interpretation(params: Spine) -> Result<RcValue, ()> {
                    if params.len() == count!($($param_name)*) {
                        let mut arg_index = 0;
                        $(
                            arg_index += 1;
                            let $param_name = <$PType>::try_from_value_ref(&params[arg_index - 1])?;
                        )*
                        Ok(<$RType>::into_value($body))
                    } else {
                        Err(()) // TODO: Better errors
                    }
                }

                PrimFn {
                    arity: count!($($param_name)*),
                    interpretation,
                }
            }};
        }

        let definitions = hashmap!{
            "string-eq".to_string() => prim!(fn(x: String, y: String) -> bool { x == y }),
            "bool-eq".to_string() => prim!(fn(x: bool, y: bool) -> bool { x == y }),
            "char-eq".to_string() => prim!(fn(x: char, y: char) -> bool { x == y }),
            "u8-eq".to_string() => prim!(fn(x: u8, y: u8) -> bool { x == y }),
            "u16-eq".to_string() => prim!(fn(x: u16, y: u16) -> bool { x == y }),
            "u32-eq".to_string() => prim!(fn(x: u32, y: u32) -> bool { x == y }),
            "u64-eq".to_string() => prim!(fn(x: u64, y: u64) -> bool { x == y }),
            "i8-eq".to_string() => prim!(fn(x: i8, y: i8) -> bool { x == y }),
            "i16-eq".to_string() => prim!(fn(x: i16, y: i16) -> bool { x == y }),
            "i32-eq".to_string() => prim!(fn(x: i32, y: i32) -> bool { x == y }),
            "i64-eq".to_string() => prim!(fn(x: i64, y: i64) -> bool { x == y }),
            "f32-eq".to_string() => prim!(fn(x: f32, y: f32) -> bool { f32::eq(x, y) }),
            "f64-eq".to_string() => prim!(fn(x: f64, y: f64) -> bool { f64::eq(x, y) }),

            "string-ne".to_string() => prim!(fn(x: String, y: String) -> bool { x != y }),
            "bool-ne".to_string() => prim!(fn(x: bool, y: bool) -> bool { x != y }),
            "char-ne".to_string() => prim!(fn(x: char, y: char) -> bool { x != y }),
            "u8-ne".to_string() => prim!(fn(x: u8, y: u8) -> bool { x != y }),
            "u16-ne".to_string() => prim!(fn(x: u16, y: u16) -> bool { x != y }),
            "u32-ne".to_string() => prim!(fn(x: u32, y: u32) -> bool { x != y }),
            "u64-ne".to_string() => prim!(fn(x: u64, y: u64) -> bool { x != y }),
            "i8-ne".to_string() => prim!(fn(x: i8, y: i8) -> bool { x != y }),
            "i16-ne".to_string() => prim!(fn(x: i16, y: i16) -> bool { x != y }),
            "i32-ne".to_string() => prim!(fn(x: i32, y: i32) -> bool { x != y }),
            "i64-ne".to_string() => prim!(fn(x: i64, y: i64) -> bool { x != y }),
            "f32-ne".to_string() => prim!(fn(x: f32, y: f32) -> bool { f32::ne(x, y) }),
            "f64-ne".to_string() => prim!(fn(x: f64, y: f64) -> bool { f64::ne(x, y) }),

            "string-le".to_string() => prim!(fn(x: String, y: String) -> bool { x <= y }),
            "bool-le".to_string() => prim!(fn(x: bool, y: bool) -> bool { x <= y }),
            "char-le".to_string() => prim!(fn(x: char, y: char) -> bool { x <= y }),
            "u8-le".to_string() => prim!(fn(x: u8, y: u8) -> bool { x <= y }),
            "u16-le".to_string() => prim!(fn(x: u16, y: u16) -> bool { x <= y }),
            "u32-le".to_string() => prim!(fn(x: u32, y: u32) -> bool { x <= y }),
            "u64-le".to_string() => prim!(fn(x: u64, y: u64) -> bool { x <= y }),
            "i8-le".to_string() => prim!(fn(x: i8, y: i8) -> bool { x <= y }),
            "i16-le".to_string() => prim!(fn(x: i16, y: i16) -> bool { x <= y }),
            "i32-le".to_string() => prim!(fn(x: i32, y: i32) -> bool { x <= y }),
            "i64-le".to_string() => prim!(fn(x: i64, y: i64) -> bool { x <= y }),
            "f32-le".to_string() => prim!(fn(x: f32, y: f32) -> bool { x <= y }),
            "f64-le".to_string() => prim!(fn(x: f64, y: f64) -> bool { x <= y }),

            "string-lt".to_string() => prim!(fn(x: String, y: String) -> bool { x < y }),
            "bool-lt".to_string() => prim!(fn(x: bool, y: bool) -> bool { x < y }),
            "char-lt".to_string() => prim!(fn(x: char, y: char) -> bool { x < y }),
            "u8-lt".to_string() => prim!(fn(x: u8, y: u8) -> bool { x < y }),
            "u16-lt".to_string() => prim!(fn(x: u16, y: u16) -> bool { x < y }),
            "u32-lt".to_string() => prim!(fn(x: u32, y: u32) -> bool { x < y }),
            "u64-lt".to_string() => prim!(fn(x: u64, y: u64) -> bool { x < y }),
            "i8-lt".to_string() => prim!(fn(x: i8, y: i8) -> bool { x < y }),
            "i16-lt".to_string() => prim!(fn(x: i16, y: i16) -> bool { x < y }),
            "i32-lt".to_string() => prim!(fn(x: i32, y: i32) -> bool { x < y }),
            "i64-lt".to_string() => prim!(fn(x: i64, y: i64) -> bool { x < y }),
            "f32-lt".to_string() => prim!(fn(x: f32, y: f32) -> bool { x < y }),
            "f64-lt".to_string() => prim!(fn(x: f64, y: f64) -> bool { x < y }),

            "string-gt".to_string() => prim!(fn(x: String, y: String) -> bool { x > y }),
            "bool-gt".to_string() => prim!(fn(x: bool, y: bool) -> bool { x > y }),
            "char-gt".to_string() => prim!(fn(x: char, y: char) -> bool { x > y }),
            "u8-gt".to_string() => prim!(fn(x: u8, y: u8) -> bool { x > y }),
            "u16-gt".to_string() => prim!(fn(x: u16, y: u16) -> bool { x > y }),
            "u32-gt".to_string() => prim!(fn(x: u32, y: u32) -> bool { x > y }),
            "u64-gt".to_string() => prim!(fn(x: u64, y: u64) -> bool { x > y }),
            "i8-gt".to_string() => prim!(fn(x: i8, y: i8) -> bool { x > y }),
            "i16-gt".to_string() => prim!(fn(x: i16, y: i16) -> bool { x > y }),
            "i32-gt".to_string() => prim!(fn(x: i32, y: i32) -> bool { x > y }),
            "i64-gt".to_string() => prim!(fn(x: i64, y: i64) -> bool { x > y }),
            "f32-gt".to_string() => prim!(fn(x: f32, y: f32) -> bool { x > y }),
            "f64-gt".to_string() => prim!(fn(x: f64, y: f64) -> bool { x > y }),

            "string-ge".to_string() => prim!(fn(x: String, y: String) -> bool { x >= y }),
            "bool-ge".to_string() => prim!(fn(x: bool, y: bool) -> bool { x >= y }),
            "char-ge".to_string() => prim!(fn(x: char, y: char) -> bool { x >= y }),
            "u8-ge".to_string() => prim!(fn(x: u8, y: u8) -> bool { x >= y }),
            "u16-ge".to_string() => prim!(fn(x: u16, y: u16) -> bool { x >= y }),
            "u32-ge".to_string() => prim!(fn(x: u32, y: u32) -> bool { x >= y }),
            "u64-ge".to_string() => prim!(fn(x: u64, y: u64) -> bool { x >= y }),
            "i8-ge".to_string() => prim!(fn(x: i8, y: i8) -> bool { x >= y }),
            "i16-ge".to_string() => prim!(fn(x: i16, y: i16) -> bool { x >= y }),
            "i32-ge".to_string() => prim!(fn(x: i32, y: i32) -> bool { x >= y }),
            "i64-ge".to_string() => prim!(fn(x: i64, y: i64) -> bool { x >= y }),
            "f32-ge".to_string() => prim!(fn(x: f32, y: f32) -> bool { x >= y }),
            "f64-ge".to_string() => prim!(fn(x: f64, y: f64) -> bool { x >= y }),

            "u8-add".to_string() => prim!(fn(x: u8, y: u8) -> u8 { x + y }),
            "u16-add".to_string() => prim!(fn(x: u16, y: u16) -> u16 { x + y }),
            "u32-add".to_string() => prim!(fn(x: u32, y: u32) -> u32 { x + y }),
            "u64-add".to_string() => prim!(fn(x: u64, y: u64) -> u64 { x + y }),
            "i8-add".to_string() => prim!(fn(x: i8, y: i8) -> i8 { x + y }),
            "i16-add".to_string() => prim!(fn(x: i16, y: i16) -> i16 { x + y }),
            "i32-add".to_string() => prim!(fn(x: i32, y: i32) -> i32 { x + y }),
            "i64-add".to_string() => prim!(fn(x: i64, y: i64) -> i64 { x + y }),
            "f32-add".to_string() => prim!(fn(x: f32, y: f32) -> f32 { x + y }),
            "f64-add".to_string() => prim!(fn(x: f64, y: f64) -> f64 { x + y }),

            "u8-sub".to_string() => prim!(fn(x: u8, y: u8) -> u8 { x - y }),
            "u16-sub".to_string() => prim!(fn(x: u16, y: u16) -> u16 { x - y }),
            "u32-sub".to_string() => prim!(fn(x: u32, y: u32) -> u32 { x - y }),
            "u64-sub".to_string() => prim!(fn(x: u64, y: u64) -> u64 { x - y }),
            "i8-sub".to_string() => prim!(fn(x: i8, y: i8) -> i8 { x - y }),
            "i16-sub".to_string() => prim!(fn(x: i16, y: i16) -> i16 { x - y }),
            "i32-sub".to_string() => prim!(fn(x: i32, y: i32) -> i32 { x - y }),
            "i64-sub".to_string() => prim!(fn(x: i64, y: i64) -> i64 { x - y }),
            "f32-sub".to_string() => prim!(fn(x: f32, y: f32) -> f32 { x - y }),
            "f64-sub".to_string() => prim!(fn(x: f64, y: f64) -> f64 { x - y }),

            "u8-mul".to_string() => prim!(fn(x: u8, y: u8) -> u8 { x * y }),
            "u16-mul".to_string() => prim!(fn(x: u16, y: u16) -> u16 { x * y }),
            "u32-mul".to_string() => prim!(fn(x: u32, y: u32) -> u32 { x * y }),
            "u64-mul".to_string() => prim!(fn(x: u64, y: u64) -> u64 { x * y }),
            "i8-mul".to_string() => prim!(fn(x: i8, y: i8) -> i8 { x * y }),
            "i16-mul".to_string() => prim!(fn(x: i16, y: i16) -> i16 { x * y }),
            "i32-mul".to_string() => prim!(fn(x: i32, y: i32) -> i32 { x * y }),
            "i64-mul".to_string() => prim!(fn(x: i64, y: i64) -> i64 { x * y }),
            "f32-mul".to_string() => prim!(fn(x: f32, y: f32) -> f32 { x * y }),
            "f64-mul".to_string() => prim!(fn(x: f64, y: f64) -> f64 { x * y }),

            "u8-div".to_string() => prim!(fn(x: u8, y: u8) -> u8 { x / y }),
            "u16-div".to_string() => prim!(fn(x: u16, y: u16) -> u16 { x / y }),
            "u32-div".to_string() => prim!(fn(x: u32, y: u32) -> u32 { x / y }),
            "u64-div".to_string() => prim!(fn(x: u64, y: u64) -> u64 { x / y }),
            "i8-div".to_string() => prim!(fn(x: i8, y: i8) -> i8 { x / y }),
            "i16-div".to_string() => prim!(fn(x: i16, y: i16) -> i16 { x / y }),
            "i32-div".to_string() => prim!(fn(x: i32, y: i32) -> i32 { x / y }),
            "i64-div".to_string() => prim!(fn(x: i64, y: i64) -> i64 { x / y }),
            "f32-div".to_string() => prim!(fn(x: f32, y: f32) -> f32 { x / y }),
            "f64-div".to_string() => prim!(fn(x: f64, y: f64) -> f64 { x / y }),

            "char-to-string".to_string() => prim!(fn(val: char) -> String { val.to_string() }),
            "u8-to-string".to_string() => prim!(fn(val: u8) -> String { val.to_string() }),
            "u16-to-string".to_string() => prim!(fn(val: u16) -> String { val.to_string() }),
            "u32-to-string".to_string() => prim!(fn(val: u32) -> String { val.to_string() }),
            "u64-to-string".to_string() => prim!(fn(val: u64) -> String { val.to_string() }),
            "i8-to-string".to_string() => prim!(fn(val: i8) -> String { val.to_string() }),
            "i16-to-string".to_string() => prim!(fn(val: i16) -> String { val.to_string() }),
            "i32-to-string".to_string() => prim!(fn(val: i32) -> String { val.to_string() }),
            "i64-to-string".to_string() => prim!(fn(val: i64) -> String { val.to_string() }),
            "f32-to-string".to_string() => prim!(fn(val: f32) -> String { val.to_string() }),
            "f64-to-string".to_string() => prim!(fn(val: f64) -> String { val.to_string() }),

            "string-append".to_string() => prim!(fn(x: String, y: String) -> String { x.clone() + y }), // FIXME: Clone
        };

        PrimEnv { definitions }
    }
}
