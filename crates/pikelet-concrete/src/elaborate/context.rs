use im;
use moniker::{Binder, FreeVar, Var};
use std::rc::Rc;

use pikelet_core::nbe;
use pikelet_core::syntax::core::{Literal, RcTerm};
use pikelet_core::syntax::domain::{RcType, RcValue, Value};
use pikelet_core::syntax::Import;
use pikelet_core::syntax::{FloatFormat, IntFormat};

use resugar::{Resugar, ResugarEnv};

// Some helper traits for marshalling between Rust and Pikelet values
//
// I'm not super happy with the API at the moment, so these are currently private

trait IntoValue {
    fn ty(context: &Context) -> RcType;
    fn into_value(self) -> RcValue;
}

macro_rules! impl_into_value {
    ($T:ty, $ty:ident, $Variant:ident) => {
        impl IntoValue for $T {
            fn ty(context: &Context) -> RcType {
                context.$ty().clone()
            }

            fn into_value(self) -> RcValue {
                RcValue::from(Value::Literal(Literal::$Variant(self)))
            }
        }
    };
    ($T:ty, $ty:ident, $Variant:ident, $format:expr) => {
        impl IntoValue for $T {
            fn ty(context: &Context) -> RcType {
                context.$ty().clone()
            }

            fn into_value(self) -> RcValue {
                RcValue::from(Value::Literal(Literal::$Variant(self, $format)))
            }
        }
    };
}

impl_into_value!(String, string, String);
impl_into_value!(char, char, Char);
impl_into_value!(bool, bool, Bool);
impl_into_value!(u8, u8, U8, IntFormat::Dec);
impl_into_value!(u16, u16, U16, IntFormat::Dec);
impl_into_value!(u32, u32, U32, IntFormat::Dec);
impl_into_value!(u64, u64, U64, IntFormat::Dec);
impl_into_value!(i8, s8, S8, IntFormat::Dec);
impl_into_value!(i16, s16, S16, IntFormat::Dec);
impl_into_value!(i32, s32, S32, IntFormat::Dec);
impl_into_value!(i64, s64, S64, IntFormat::Dec);
impl_into_value!(f32, f32, F32, FloatFormat::Dec);
impl_into_value!(f64, f64, F64, FloatFormat::Dec);

trait TryFromValueRef {
    fn try_from_value_ref(src: &Value) -> Option<&Self>;
}

impl TryFromValueRef for String {
    fn try_from_value_ref(src: &Value) -> Option<&Self> {
        match *src {
            Value::Literal(Literal::String(ref val)) => Some(val),
            _ => None,
        }
    }
}

impl TryFromValueRef for char {
    fn try_from_value_ref(src: &Value) -> Option<&Self> {
        match *src {
            Value::Literal(Literal::Char(ref val)) => Some(val),
            _ => None,
        }
    }
}

impl TryFromValueRef for bool {
    fn try_from_value_ref(src: &Value) -> Option<&Self> {
        match *src {
            Value::Literal(Literal::Bool(ref val)) => Some(val),
            _ => None,
        }
    }
}

impl TryFromValueRef for u8 {
    fn try_from_value_ref(src: &Value) -> Option<&Self> {
        match *src {
            Value::Literal(Literal::U8(ref val, _)) => Some(val),
            _ => None,
        }
    }
}

impl TryFromValueRef for u16 {
    fn try_from_value_ref(src: &Value) -> Option<&Self> {
        match *src {
            Value::Literal(Literal::U16(ref val, _)) => Some(val),
            _ => None,
        }
    }
}

impl TryFromValueRef for u32 {
    fn try_from_value_ref(src: &Value) -> Option<&Self> {
        match *src {
            Value::Literal(Literal::U32(ref val, _)) => Some(val),
            _ => None,
        }
    }
}

impl TryFromValueRef for u64 {
    fn try_from_value_ref(src: &Value) -> Option<&Self> {
        match *src {
            Value::Literal(Literal::U64(ref val, _)) => Some(val),
            _ => None,
        }
    }
}

impl TryFromValueRef for i8 {
    fn try_from_value_ref(src: &Value) -> Option<&Self> {
        match *src {
            Value::Literal(Literal::S8(ref val, _)) => Some(val),
            _ => None,
        }
    }
}

impl TryFromValueRef for i16 {
    fn try_from_value_ref(src: &Value) -> Option<&Self> {
        match *src {
            Value::Literal(Literal::S16(ref val, _)) => Some(val),
            _ => None,
        }
    }
}

impl TryFromValueRef for i32 {
    fn try_from_value_ref(src: &Value) -> Option<&Self> {
        match *src {
            Value::Literal(Literal::S32(ref val, _)) => Some(val),
            _ => None,
        }
    }
}

impl TryFromValueRef for i64 {
    fn try_from_value_ref(src: &Value) -> Option<&Self> {
        match *src {
            Value::Literal(Literal::S64(ref val, _)) => Some(val),
            _ => None,
        }
    }
}

impl TryFromValueRef for f32 {
    fn try_from_value_ref(src: &Value) -> Option<&Self> {
        match *src {
            Value::Literal(Literal::F32(ref val, _)) => Some(val),
            _ => None,
        }
    }
}

impl TryFromValueRef for f64 {
    fn try_from_value_ref(src: &Value) -> Option<&Self> {
        match *src {
            Value::Literal(Literal::F64(ref val, _)) => Some(val),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Globals {
    ty_bool: RcType,
    ty_string: RcType,
    ty_char: RcType,
    ty_u8: RcType,
    ty_u16: RcType,
    ty_u32: RcType,
    ty_u64: RcType,
    ty_s8: RcType,
    ty_s16: RcType,
    ty_s32: RcType,
    ty_s64: RcType,
    ty_f32: RcType,
    ty_f64: RcType,
    var_array: FreeVar<String>,
}

/// The type checking context
///
/// A default context with entries for built-in types is provided via the
/// implementation of the `Default` trait.
///
/// We use persistent data structures internally so that we can copy the
/// context as we enter into scopes, without having to deal with the
/// error-prone tedium of working with mutable context.
#[derive(Clone, Debug)]
pub struct Context {
    /// The resugar context
    ///
    /// We'll keep this up to date as we type check to make it easier to do
    /// resugaring on any errors that we encounter
    resugar_env: ResugarEnv,
    /// The globals
    globals: Rc<Globals>,
    /// Imports
    imports: im::HashMap<String, (Import, RcType)>,
    /// The type annotations of the binders we have passed over
    declarations: im::HashMap<FreeVar<String>, RcType>,
    /// Any definitions we have passed over
    definitions: im::HashMap<FreeVar<String>, RcTerm>,
}

impl Default for Context {
    fn default() -> Context {
        use moniker::{Embed, Scope};

        use pikelet_core::syntax::core::Term;

        let var_bool = FreeVar::fresh_named("Bool");
        let var_true = FreeVar::fresh_named("true");
        let var_false = FreeVar::fresh_named("false");
        let var_string = FreeVar::fresh_named("String");
        let var_char = FreeVar::fresh_named("Char");
        let var_u8 = FreeVar::fresh_named("U8");
        let var_u16 = FreeVar::fresh_named("U16");
        let var_u32 = FreeVar::fresh_named("U32");
        let var_u64 = FreeVar::fresh_named("U64");
        let var_s8 = FreeVar::fresh_named("S8");
        let var_s16 = FreeVar::fresh_named("S16");
        let var_s32 = FreeVar::fresh_named("S32");
        let var_s64 = FreeVar::fresh_named("S64");
        let var_f32 = FreeVar::fresh_named("F32");
        let var_f64 = FreeVar::fresh_named("F64");
        let var_array = FreeVar::fresh_named("Array");

        let mut context = Context {
            resugar_env: ResugarEnv::new(),
            globals: Rc::new(Globals {
                ty_bool: RcValue::from(Value::var(Var::Free(var_bool.clone()), 0)),
                ty_string: RcValue::from(Value::var(Var::Free(var_string.clone()), 0)),
                ty_char: RcValue::from(Value::var(Var::Free(var_char.clone()), 0)),
                ty_u8: RcValue::from(Value::var(Var::Free(var_u8.clone()), 0)),
                ty_u16: RcValue::from(Value::var(Var::Free(var_u16.clone()), 0)),
                ty_u32: RcValue::from(Value::var(Var::Free(var_u32.clone()), 0)),
                ty_u64: RcValue::from(Value::var(Var::Free(var_u64.clone()), 0)),
                ty_s8: RcValue::from(Value::var(Var::Free(var_s8.clone()), 0)),
                ty_s16: RcValue::from(Value::var(Var::Free(var_s16.clone()), 0)),
                ty_s32: RcValue::from(Value::var(Var::Free(var_s32.clone()), 0)),
                ty_s64: RcValue::from(Value::var(Var::Free(var_s64.clone()), 0)),
                ty_f32: RcValue::from(Value::var(Var::Free(var_f32.clone()), 0)),
                ty_f64: RcValue::from(Value::var(Var::Free(var_f64.clone()), 0)),
                var_array: var_array.clone(),
            }),
            imports: im::HashMap::new(),
            declarations: im::HashMap::new(),
            definitions: im::HashMap::new(),
        };

        let universe0 = RcValue::from(Value::universe(0));
        let bool_ty = context.globals.ty_bool.clone();
        let bool_lit = |value| RcTerm::from(Term::Literal(Literal::Bool(value)));
        let array_ty = RcValue::from(Value::FunType(Scope::new(
            (
                Binder(FreeVar::fresh_unnamed()),
                Embed(context.globals.ty_u64.clone()),
            ),
            RcValue::from(Value::FunType(Scope::new(
                (Binder(FreeVar::fresh_unnamed()), Embed(universe0.clone())),
                universe0.clone(),
            ))),
        )));

        context.insert_declaration(var_bool, universe0.clone());
        context.insert_declaration(var_string, universe0.clone());
        context.insert_declaration(var_char, universe0.clone());
        context.insert_declaration(var_u8, universe0.clone());
        context.insert_declaration(var_u16, universe0.clone());
        context.insert_declaration(var_u32, universe0.clone());
        context.insert_declaration(var_u64, universe0.clone());
        context.insert_declaration(var_s8, universe0.clone());
        context.insert_declaration(var_s16, universe0.clone());
        context.insert_declaration(var_s32, universe0.clone());
        context.insert_declaration(var_s64, universe0.clone());
        context.insert_declaration(var_f32, universe0.clone());
        context.insert_declaration(var_f64, universe0.clone());
        context.insert_declaration(var_array, array_ty);

        context.insert_declaration(var_true.clone(), bool_ty.clone());
        context.insert_declaration(var_false.clone(), bool_ty.clone());
        context.insert_definition(var_true, bool_lit(true));
        context.insert_definition(var_false, bool_lit(false));

        /// Define a primitive import
        macro_rules! prim_import {
            ($name:expr, fn($($param_name:ident : $PType:ty),*) -> $RType:ty $body:block) => {{
                fn interpretation<'a>(params: &'a [RcValue]) -> Option<RcValue> {
                    match params {
                        [$(ref $param_name),*] if $($param_name.is_nf())&&* => {
                            $(let $param_name = <$PType>::try_from_value_ref($param_name)?;)*
                            Some(<$RType>::into_value($body))
                        }
                        _ => None,
                    }
                }

                let ty = <$RType>::ty(&context);
                $(let ty = {
                    let param_var = FreeVar::fresh_unnamed();
                    let param_ty = <$PType>::ty(&context);
                    RcValue::from(Value::FunType(Scope::new((Binder(param_var), Embed(param_ty)), ty)))
                };)*

                context.insert_import($name.to_owned(), Import::Prim(interpretation), ty);
            }};
        }

        prim_import!("prim/string/eq", fn(x: String, y: String) -> bool { x == y });
        prim_import!("prim/bool/eq", fn(x: bool, y: bool) -> bool { x == y });
        prim_import!("prim/char/eq", fn(x: char, y: char) -> bool { x == y });
        prim_import!("prim/u8/eq", fn(x: u8, y: u8) -> bool { x == y });
        prim_import!("prim/u16/eq", fn(x: u16, y: u16) -> bool { x == y });
        prim_import!("prim/u32/eq", fn(x: u32, y: u32) -> bool { x == y });
        prim_import!("prim/u64/eq", fn(x: u64, y: u64) -> bool { x == y });
        prim_import!("prim/i8/eq", fn(x: i8, y: i8) -> bool { x == y });
        prim_import!("prim/i16/eq", fn(x: i16, y: i16) -> bool { x == y });
        prim_import!("prim/i32/eq", fn(x: i32, y: i32) -> bool { x == y });
        prim_import!("prim/i64/eq", fn(x: i64, y: i64) -> bool { x == y });
        prim_import!("prim/f32/eq", fn(x: f32, y: f32) -> bool { f32::eq(x, y) });
        prim_import!("prim/f64/eq", fn(x: f64, y: f64) -> bool { f64::eq(x, y) });

        prim_import!("prim/string/ne", fn(x: String, y: String) -> bool { x != y });
        prim_import!("prim/bool/ne", fn(x: bool, y: bool) -> bool { x != y });
        prim_import!("prim/char/ne", fn(x: char, y: char) -> bool { x != y });
        prim_import!("prim/u8/ne", fn(x: u8, y: u8) -> bool { x != y });
        prim_import!("prim/u16/ne", fn(x: u16, y: u16) -> bool { x != y });
        prim_import!("prim/u32/ne", fn(x: u32, y: u32) -> bool { x != y });
        prim_import!("prim/u64/ne", fn(x: u64, y: u64) -> bool { x != y });
        prim_import!("prim/i8/ne", fn(x: i8, y: i8) -> bool { x != y });
        prim_import!("prim/i16/ne", fn(x: i16, y: i16) -> bool { x != y });
        prim_import!("prim/i32/ne", fn(x: i32, y: i32) -> bool { x != y });
        prim_import!("prim/i64/ne", fn(x: i64, y: i64) -> bool { x != y });
        prim_import!("prim/f32/ne", fn(x: f32, y: f32) -> bool { f32::ne(x, y) });
        prim_import!("prim/f64/ne", fn(x: f64, y: f64) -> bool { f64::ne(x, y) });

        prim_import!("prim/string/le", fn(x: String, y: String) -> bool { x <= y });
        prim_import!("prim/bool/le", fn(x: bool, y: bool) -> bool { x <= y });
        prim_import!("prim/char/le", fn(x: char, y: char) -> bool { x <= y });
        prim_import!("prim/u8/le", fn(x: u8, y: u8) -> bool { x <= y });
        prim_import!("prim/u16/le", fn(x: u16, y: u16) -> bool { x <= y });
        prim_import!("prim/u32/le", fn(x: u32, y: u32) -> bool { x <= y });
        prim_import!("prim/u64/le", fn(x: u64, y: u64) -> bool { x <= y });
        prim_import!("prim/i8/le", fn(x: i8, y: i8) -> bool { x <= y });
        prim_import!("prim/i16/le", fn(x: i16, y: i16) -> bool { x <= y });
        prim_import!("prim/i32/le", fn(x: i32, y: i32) -> bool { x <= y });
        prim_import!("prim/i64/le", fn(x: i64, y: i64) -> bool { x <= y });
        prim_import!("prim/f32/le", fn(x: f32, y: f32) -> bool { x <= y });
        prim_import!("prim/f64/le", fn(x: f64, y: f64) -> bool { x <= y });

        prim_import!("prim/string/lt", fn(x: String, y: String) -> bool { x < y });
        prim_import!("prim/bool/lt", fn(x: bool, y: bool) -> bool { x < y });
        prim_import!("prim/char/lt", fn(x: char, y: char) -> bool { x < y });
        prim_import!("prim/u8/lt", fn(x: u8, y: u8) -> bool { x < y });
        prim_import!("prim/u16/lt", fn(x: u16, y: u16) -> bool { x < y });
        prim_import!("prim/u32/lt", fn(x: u32, y: u32) -> bool { x < y });
        prim_import!("prim/u64/lt", fn(x: u64, y: u64) -> bool { x < y });
        prim_import!("prim/i8/lt", fn(x: i8, y: i8) -> bool { x < y });
        prim_import!("prim/i16/lt", fn(x: i16, y: i16) -> bool { x < y });
        prim_import!("prim/i32/lt", fn(x: i32, y: i32) -> bool { x < y });
        prim_import!("prim/i64/lt", fn(x: i64, y: i64) -> bool { x < y });
        prim_import!("prim/f32/lt", fn(x: f32, y: f32) -> bool { x < y });
        prim_import!("prim/f64/lt", fn(x: f64, y: f64) -> bool { x < y });

        prim_import!("prim/string/gt", fn(x: String, y: String) -> bool { x > y });
        prim_import!("prim/bool/gt", fn(x: bool, y: bool) -> bool { x > y });
        prim_import!("prim/char/gt", fn(x: char, y: char) -> bool { x > y });
        prim_import!("prim/u8/gt", fn(x: u8, y: u8) -> bool { x > y });
        prim_import!("prim/u16/gt", fn(x: u16, y: u16) -> bool { x > y });
        prim_import!("prim/u32/gt", fn(x: u32, y: u32) -> bool { x > y });
        prim_import!("prim/u64/gt", fn(x: u64, y: u64) -> bool { x > y });
        prim_import!("prim/i8/gt", fn(x: i8, y: i8) -> bool { x > y });
        prim_import!("prim/i16/gt", fn(x: i16, y: i16) -> bool { x > y });
        prim_import!("prim/i32/gt", fn(x: i32, y: i32) -> bool { x > y });
        prim_import!("prim/i64/gt", fn(x: i64, y: i64) -> bool { x > y });
        prim_import!("prim/f32/gt", fn(x: f32, y: f32) -> bool { x > y });
        prim_import!("prim/f64/gt", fn(x: f64, y: f64) -> bool { x > y });

        prim_import!("prim/string/ge", fn(x: String, y: String) -> bool { x >= y });
        prim_import!("prim/bool/ge", fn(x: bool, y: bool) -> bool { x >= y });
        prim_import!("prim/char/ge", fn(x: char, y: char) -> bool { x >= y });
        prim_import!("prim/u8/ge", fn(x: u8, y: u8) -> bool { x >= y });
        prim_import!("prim/u16/ge", fn(x: u16, y: u16) -> bool { x >= y });
        prim_import!("prim/u32/ge", fn(x: u32, y: u32) -> bool { x >= y });
        prim_import!("prim/u64/ge", fn(x: u64, y: u64) -> bool { x >= y });
        prim_import!("prim/i8/ge", fn(x: i8, y: i8) -> bool { x >= y });
        prim_import!("prim/i16/ge", fn(x: i16, y: i16) -> bool { x >= y });
        prim_import!("prim/i32/ge", fn(x: i32, y: i32) -> bool { x >= y });
        prim_import!("prim/i64/ge", fn(x: i64, y: i64) -> bool { x >= y });
        prim_import!("prim/f32/ge", fn(x: f32, y: f32) -> bool { x >= y });
        prim_import!("prim/f64/ge", fn(x: f64, y: f64) -> bool { x >= y });

        prim_import!("prim/u8/add", fn(x: u8, y: u8) -> u8 { x + y });
        prim_import!("prim/u16/add", fn(x: u16, y: u16) -> u16 { x + y });
        prim_import!("prim/u32/add", fn(x: u32, y: u32) -> u32 { x + y });
        prim_import!("prim/u64/add", fn(x: u64, y: u64) -> u64 { x + y });
        prim_import!("prim/i8/add", fn(x: i8, y: i8) -> i8 { x + y });
        prim_import!("prim/i16/add", fn(x: i16, y: i16) -> i16 { x + y });
        prim_import!("prim/i32/add", fn(x: i32, y: i32) -> i32 { x + y });
        prim_import!("prim/i64/add", fn(x: i64, y: i64) -> i64 { x + y });
        prim_import!("prim/f32/add", fn(x: f32, y: f32) -> f32 { x + y });
        prim_import!("prim/f64/add", fn(x: f64, y: f64) -> f64 { x + y });

        prim_import!("prim/u8/sub", fn(x: u8, y: u8) -> u8 { x - y });
        prim_import!("prim/u16/sub", fn(x: u16, y: u16) -> u16 { x - y });
        prim_import!("prim/u32/sub", fn(x: u32, y: u32) -> u32 { x - y });
        prim_import!("prim/u64/sub", fn(x: u64, y: u64) -> u64 { x - y });
        prim_import!("prim/i8/sub", fn(x: i8, y: i8) -> i8 { x - y });
        prim_import!("prim/i16/sub", fn(x: i16, y: i16) -> i16 { x - y });
        prim_import!("prim/i32/sub", fn(x: i32, y: i32) -> i32 { x - y });
        prim_import!("prim/i64/sub", fn(x: i64, y: i64) -> i64 { x - y });
        prim_import!("prim/f32/sub", fn(x: f32, y: f32) -> f32 { x - y });
        prim_import!("prim/f64/sub", fn(x: f64, y: f64) -> f64 { x - y });

        prim_import!("prim/u8/mul", fn(x: u8, y: u8) -> u8 { x * y });
        prim_import!("prim/u16/mul", fn(x: u16, y: u16) -> u16 { x * y });
        prim_import!("prim/u32/mul", fn(x: u32, y: u32) -> u32 { x * y });
        prim_import!("prim/u64/mul", fn(x: u64, y: u64) -> u64 { x * y });
        prim_import!("prim/i8/mul", fn(x: i8, y: i8) -> i8 { x * y });
        prim_import!("prim/i16/mul", fn(x: i16, y: i16) -> i16 { x * y });
        prim_import!("prim/i32/mul", fn(x: i32, y: i32) -> i32 { x * y });
        prim_import!("prim/i64/mul", fn(x: i64, y: i64) -> i64 { x * y });
        prim_import!("prim/f32/mul", fn(x: f32, y: f32) -> f32 { x * y });
        prim_import!("prim/f64/mul", fn(x: f64, y: f64) -> f64 { x * y });

        prim_import!("prim/u8/div", fn(x: u8, y: u8) -> u8 { x / y });
        prim_import!("prim/u16/div", fn(x: u16, y: u16) -> u16 { x / y });
        prim_import!("prim/u32/div", fn(x: u32, y: u32) -> u32 { x / y });
        prim_import!("prim/u64/div", fn(x: u64, y: u64) -> u64 { x / y });
        prim_import!("prim/i8/div", fn(x: i8, y: i8) -> i8 { x / y });
        prim_import!("prim/i16/div", fn(x: i16, y: i16) -> i16 { x / y });
        prim_import!("prim/i32/div", fn(x: i32, y: i32) -> i32 { x / y });
        prim_import!("prim/i64/div", fn(x: i64, y: i64) -> i64 { x / y });
        prim_import!("prim/f32/div", fn(x: f32, y: f32) -> f32 { x / y });
        prim_import!("prim/f64/div", fn(x: f64, y: f64) -> f64 { x / y });

        prim_import!("prim/char/to-string", fn(val: char) -> String { val.to_string() });
        prim_import!("prim/u8/to-string", fn(val: u8) -> String { val.to_string() });
        prim_import!("prim/u16/to-string", fn(val: u16) -> String { val.to_string() });
        prim_import!("prim/u32/to-string", fn(val: u32) -> String { val.to_string() });
        prim_import!("prim/u64/to-string", fn(val: u64) -> String { val.to_string() });
        prim_import!("prim/i8/to-string", fn(val: i8) -> String { val.to_string() });
        prim_import!("prim/i16/to-string", fn(val: i16) -> String { val.to_string() });
        prim_import!("prim/i32/to-string", fn(val: i32) -> String { val.to_string() });
        prim_import!("prim/i64/to-string", fn(val: i64) -> String { val.to_string() });
        prim_import!("prim/f32/to-string", fn(val: f32) -> String { val.to_string() });
        prim_import!("prim/f64/to-string", fn(val: f64) -> String { val.to_string() });

        prim_import!("prim/string/append", fn(x: String, y: String) -> String { x.clone() + y }); // FIXME: Clone

        context
    }
}

impl Context {
    pub fn resugar<T>(&self, src: &impl Resugar<T>) -> T {
        src.resugar(&self.resugar_env)
    }

    pub fn mappings(&self) -> im::HashMap<String, FreeVar<String>> {
        self.declarations
            .iter()
            .filter_map(|(free_var, _)| {
                let pretty_name = free_var.pretty_name.as_ref()?;
                Some((pretty_name.clone(), free_var.clone()))
            })
            .collect()
    }

    pub fn bool(&self) -> &RcType {
        &self.globals.ty_bool
    }

    pub fn string(&self) -> &RcType {
        &self.globals.ty_string
    }

    pub fn char(&self) -> &RcType {
        &self.globals.ty_char
    }

    pub fn u8(&self) -> &RcType {
        &self.globals.ty_u8
    }

    pub fn u16(&self) -> &RcType {
        &self.globals.ty_u16
    }

    pub fn u32(&self) -> &RcType {
        &self.globals.ty_u32
    }

    pub fn u64(&self) -> &RcType {
        &self.globals.ty_u64
    }

    pub fn s8(&self) -> &RcType {
        &self.globals.ty_s8
    }

    pub fn s16(&self) -> &RcType {
        &self.globals.ty_s16
    }

    pub fn s32(&self) -> &RcType {
        &self.globals.ty_s32
    }

    pub fn s64(&self) -> &RcType {
        &self.globals.ty_s64
    }

    pub fn f32(&self) -> &RcType {
        &self.globals.ty_f32
    }

    pub fn f64(&self) -> &RcType {
        &self.globals.ty_f64
    }

    pub fn array<'a>(&self, ty: &'a RcType) -> Option<(u64, &'a RcType)> {
        use pikelet_core::syntax::LevelShift;

        match ty.free_var_app() {
            // Conservatively forcing the shift to be zero for now. Perhaps this
            // could be relaxed in the future if it becomes a problem?
            Some((fv, LevelShift(0), &[ref len, ref elem_ty])) if *fv == self.globals.var_array => {
                match **len {
                    Value::Literal(Literal::U64(len, _)) => Some((len, elem_ty)),
                    _ => None,
                }
            },
            Some(_) | None => None,
        }
    }

    pub fn get_import(&self, name: &str) -> Option<&(Import, RcType)> {
        self.imports.get(name)
    }

    pub fn get_declaration(&self, free_var: &FreeVar<String>) -> Option<&RcType> {
        self.declarations.get(free_var)
    }

    pub fn get_definition(&self, free_var: &FreeVar<String>) -> Option<&RcTerm> {
        self.definitions.get(free_var)
    }

    pub fn insert_import(&mut self, name: String, import: Import, ty: RcType) {
        self.imports.insert(name, (import, ty));
    }

    pub fn insert_declaration(&mut self, free_var: FreeVar<String>, ty: RcType) {
        self.resugar_env.on_binder(&Binder(free_var.clone()));
        self.declarations.insert(free_var, ty);
    }

    pub fn insert_definition(&mut self, free_var: FreeVar<String>, term: RcTerm) {
        self.resugar_env.on_binder(&Binder(free_var.clone()));
        self.definitions.insert(free_var, term);
    }
}

impl nbe::Env for Context {
    fn get_import(&self, name: &str) -> Option<&Import> {
        self.imports.get(name).map(|&(ref import, _)| import)
    }

    fn get_definition(&self, free_var: &FreeVar<String>) -> Option<&RcTerm> {
        self.definitions.get(free_var)
    }
}
