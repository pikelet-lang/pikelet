use im::HashMap;
use moniker::FreeVar;
use std::fmt;
use std::rc::Rc;

use syntax::core::{Literal, RcTerm, RcType, RcValue, Spine, Value};

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

/// External functions
#[derive(Clone)]
pub struct Extern {
    /// The number of arguments to pass to the primitive during normalization
    pub arity: usize,
    /// The primitive definition to be used during normalization
    // TODO: Return a `Result` with better errors
    pub interpretation: fn(Spine) -> Result<RcValue, ()>,
}

impl fmt::Debug for Extern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Extern")
            .field("arity", &self.arity)
            .field("interpretation", &"|params| { .. }")
            .finish()
    }
}

fn default_extern_definitions() -> HashMap<&'static str, Extern> {
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

            Extern {
                arity: count!($($param_name)*),
                interpretation,
            }
        }};
    }

    hashmap!{
        "string-eq" => prim!(fn(x: String, y: String) -> bool { x == y }),
        "bool-eq" => prim!(fn(x: bool, y: bool) -> bool { x == y }),
        "char-eq" => prim!(fn(x: char, y: char) -> bool { x == y }),
        "u8-eq" => prim!(fn(x: u8, y: u8) -> bool { x == y }),
        "u16-eq" => prim!(fn(x: u16, y: u16) -> bool { x == y }),
        "u32-eq" => prim!(fn(x: u32, y: u32) -> bool { x == y }),
        "u64-eq" => prim!(fn(x: u64, y: u64) -> bool { x == y }),
        "i8-eq" => prim!(fn(x: i8, y: i8) -> bool { x == y }),
        "i16-eq" => prim!(fn(x: i16, y: i16) -> bool { x == y }),
        "i32-eq" => prim!(fn(x: i32, y: i32) -> bool { x == y }),
        "i64-eq" => prim!(fn(x: i64, y: i64) -> bool { x == y }),
        "f32-eq" => prim!(fn(x: f32, y: f32) -> bool { f32::eq(x, y) }),
        "f64-eq" => prim!(fn(x: f64, y: f64) -> bool { f64::eq(x, y) }),

        "string-ne" => prim!(fn(x: String, y: String) -> bool { x != y }),
        "bool-ne" => prim!(fn(x: bool, y: bool) -> bool { x != y }),
        "char-ne" => prim!(fn(x: char, y: char) -> bool { x != y }),
        "u8-ne" => prim!(fn(x: u8, y: u8) -> bool { x != y }),
        "u16-ne" => prim!(fn(x: u16, y: u16) -> bool { x != y }),
        "u32-ne" => prim!(fn(x: u32, y: u32) -> bool { x != y }),
        "u64-ne" => prim!(fn(x: u64, y: u64) -> bool { x != y }),
        "i8-ne" => prim!(fn(x: i8, y: i8) -> bool { x != y }),
        "i16-ne" => prim!(fn(x: i16, y: i16) -> bool { x != y }),
        "i32-ne" => prim!(fn(x: i32, y: i32) -> bool { x != y }),
        "i64-ne" => prim!(fn(x: i64, y: i64) -> bool { x != y }),
        "f32-ne" => prim!(fn(x: f32, y: f32) -> bool { f32::ne(x, y) }),
        "f64-ne" => prim!(fn(x: f64, y: f64) -> bool { f64::ne(x, y) }),

        "string-le" => prim!(fn(x: String, y: String) -> bool { x <= y }),
        "bool-le" => prim!(fn(x: bool, y: bool) -> bool { x <= y }),
        "char-le" => prim!(fn(x: char, y: char) -> bool { x <= y }),
        "u8-le" => prim!(fn(x: u8, y: u8) -> bool { x <= y }),
        "u16-le" => prim!(fn(x: u16, y: u16) -> bool { x <= y }),
        "u32-le" => prim!(fn(x: u32, y: u32) -> bool { x <= y }),
        "u64-le" => prim!(fn(x: u64, y: u64) -> bool { x <= y }),
        "i8-le" => prim!(fn(x: i8, y: i8) -> bool { x <= y }),
        "i16-le" => prim!(fn(x: i16, y: i16) -> bool { x <= y }),
        "i32-le" => prim!(fn(x: i32, y: i32) -> bool { x <= y }),
        "i64-le" => prim!(fn(x: i64, y: i64) -> bool { x <= y }),
        "f32-le" => prim!(fn(x: f32, y: f32) -> bool { x <= y }),
        "f64-le" => prim!(fn(x: f64, y: f64) -> bool { x <= y }),

        "string-lt" => prim!(fn(x: String, y: String) -> bool { x < y }),
        "bool-lt" => prim!(fn(x: bool, y: bool) -> bool { x < y }),
        "char-lt" => prim!(fn(x: char, y: char) -> bool { x < y }),
        "u8-lt" => prim!(fn(x: u8, y: u8) -> bool { x < y }),
        "u16-lt" => prim!(fn(x: u16, y: u16) -> bool { x < y }),
        "u32-lt" => prim!(fn(x: u32, y: u32) -> bool { x < y }),
        "u64-lt" => prim!(fn(x: u64, y: u64) -> bool { x < y }),
        "i8-lt" => prim!(fn(x: i8, y: i8) -> bool { x < y }),
        "i16-lt" => prim!(fn(x: i16, y: i16) -> bool { x < y }),
        "i32-lt" => prim!(fn(x: i32, y: i32) -> bool { x < y }),
        "i64-lt" => prim!(fn(x: i64, y: i64) -> bool { x < y }),
        "f32-lt" => prim!(fn(x: f32, y: f32) -> bool { x < y }),
        "f64-lt" => prim!(fn(x: f64, y: f64) -> bool { x < y }),

        "string-gt" => prim!(fn(x: String, y: String) -> bool { x > y }),
        "bool-gt" => prim!(fn(x: bool, y: bool) -> bool { x > y }),
        "char-gt" => prim!(fn(x: char, y: char) -> bool { x > y }),
        "u8-gt" => prim!(fn(x: u8, y: u8) -> bool { x > y }),
        "u16-gt" => prim!(fn(x: u16, y: u16) -> bool { x > y }),
        "u32-gt" => prim!(fn(x: u32, y: u32) -> bool { x > y }),
        "u64-gt" => prim!(fn(x: u64, y: u64) -> bool { x > y }),
        "i8-gt" => prim!(fn(x: i8, y: i8) -> bool { x > y }),
        "i16-gt" => prim!(fn(x: i16, y: i16) -> bool { x > y }),
        "i32-gt" => prim!(fn(x: i32, y: i32) -> bool { x > y }),
        "i64-gt" => prim!(fn(x: i64, y: i64) -> bool { x > y }),
        "f32-gt" => prim!(fn(x: f32, y: f32) -> bool { x > y }),
        "f64-gt" => prim!(fn(x: f64, y: f64) -> bool { x > y }),

        "string-ge" => prim!(fn(x: String, y: String) -> bool { x >= y }),
        "bool-ge" => prim!(fn(x: bool, y: bool) -> bool { x >= y }),
        "char-ge" => prim!(fn(x: char, y: char) -> bool { x >= y }),
        "u8-ge" => prim!(fn(x: u8, y: u8) -> bool { x >= y }),
        "u16-ge" => prim!(fn(x: u16, y: u16) -> bool { x >= y }),
        "u32-ge" => prim!(fn(x: u32, y: u32) -> bool { x >= y }),
        "u64-ge" => prim!(fn(x: u64, y: u64) -> bool { x >= y }),
        "i8-ge" => prim!(fn(x: i8, y: i8) -> bool { x >= y }),
        "i16-ge" => prim!(fn(x: i16, y: i16) -> bool { x >= y }),
        "i32-ge" => prim!(fn(x: i32, y: i32) -> bool { x >= y }),
        "i64-ge" => prim!(fn(x: i64, y: i64) -> bool { x >= y }),
        "f32-ge" => prim!(fn(x: f32, y: f32) -> bool { x >= y }),
        "f64-ge" => prim!(fn(x: f64, y: f64) -> bool { x >= y }),

        "u8-add" => prim!(fn(x: u8, y: u8) -> u8 { x + y }),
        "u16-add" => prim!(fn(x: u16, y: u16) -> u16 { x + y }),
        "u32-add" => prim!(fn(x: u32, y: u32) -> u32 { x + y }),
        "u64-add" => prim!(fn(x: u64, y: u64) -> u64 { x + y }),
        "i8-add" => prim!(fn(x: i8, y: i8) -> i8 { x + y }),
        "i16-add" => prim!(fn(x: i16, y: i16) -> i16 { x + y }),
        "i32-add" => prim!(fn(x: i32, y: i32) -> i32 { x + y }),
        "i64-add" => prim!(fn(x: i64, y: i64) -> i64 { x + y }),
        "f32-add" => prim!(fn(x: f32, y: f32) -> f32 { x + y }),
        "f64-add" => prim!(fn(x: f64, y: f64) -> f64 { x + y }),

        "u8-sub" => prim!(fn(x: u8, y: u8) -> u8 { x - y }),
        "u16-sub" => prim!(fn(x: u16, y: u16) -> u16 { x - y }),
        "u32-sub" => prim!(fn(x: u32, y: u32) -> u32 { x - y }),
        "u64-sub" => prim!(fn(x: u64, y: u64) -> u64 { x - y }),
        "i8-sub" => prim!(fn(x: i8, y: i8) -> i8 { x - y }),
        "i16-sub" => prim!(fn(x: i16, y: i16) -> i16 { x - y }),
        "i32-sub" => prim!(fn(x: i32, y: i32) -> i32 { x - y }),
        "i64-sub" => prim!(fn(x: i64, y: i64) -> i64 { x - y }),
        "f32-sub" => prim!(fn(x: f32, y: f32) -> f32 { x - y }),
        "f64-sub" => prim!(fn(x: f64, y: f64) -> f64 { x - y }),

        "u8-mul" => prim!(fn(x: u8, y: u8) -> u8 { x * y }),
        "u16-mul" => prim!(fn(x: u16, y: u16) -> u16 { x * y }),
        "u32-mul" => prim!(fn(x: u32, y: u32) -> u32 { x * y }),
        "u64-mul" => prim!(fn(x: u64, y: u64) -> u64 { x * y }),
        "i8-mul" => prim!(fn(x: i8, y: i8) -> i8 { x * y }),
        "i16-mul" => prim!(fn(x: i16, y: i16) -> i16 { x * y }),
        "i32-mul" => prim!(fn(x: i32, y: i32) -> i32 { x * y }),
        "i64-mul" => prim!(fn(x: i64, y: i64) -> i64 { x * y }),
        "f32-mul" => prim!(fn(x: f32, y: f32) -> f32 { x * y }),
        "f64-mul" => prim!(fn(x: f64, y: f64) -> f64 { x * y }),

        "u8-div" => prim!(fn(x: u8, y: u8) -> u8 { x / y }),
        "u16-div" => prim!(fn(x: u16, y: u16) -> u16 { x / y }),
        "u32-div" => prim!(fn(x: u32, y: u32) -> u32 { x / y }),
        "u64-div" => prim!(fn(x: u64, y: u64) -> u64 { x / y }),
        "i8-div" => prim!(fn(x: i8, y: i8) -> i8 { x / y }),
        "i16-div" => prim!(fn(x: i16, y: i16) -> i16 { x / y }),
        "i32-div" => prim!(fn(x: i32, y: i32) -> i32 { x / y }),
        "i64-div" => prim!(fn(x: i64, y: i64) -> i64 { x / y }),
        "f32-div" => prim!(fn(x: f32, y: f32) -> f32 { x / y }),
        "f64-div" => prim!(fn(x: f64, y: f64) -> f64 { x / y }),

        "char-to-string" => prim!(fn(val: char) -> String { val.to_string() }),
        "u8-to-string" => prim!(fn(val: u8) -> String { val.to_string() }),
        "u16-to-string" => prim!(fn(val: u16) -> String { val.to_string() }),
        "u32-to-string" => prim!(fn(val: u32) -> String { val.to_string() }),
        "u64-to-string" => prim!(fn(val: u64) -> String { val.to_string() }),
        "i8-to-string" => prim!(fn(val: i8) -> String { val.to_string() }),
        "i16-to-string" => prim!(fn(val: i16) -> String { val.to_string() }),
        "i32-to-string" => prim!(fn(val: i32) -> String { val.to_string() }),
        "i64-to-string" => prim!(fn(val: i64) -> String { val.to_string() }),
        "f32-to-string" => prim!(fn(val: f32) -> String { val.to_string() }),
        "f64-to-string" => prim!(fn(val: f64) -> String { val.to_string() }),

        "string-append" => prim!(fn(x: String, y: String) -> String { x.clone() + y }), // FIXME: Clone
    }
}

fn default_declarations(globals: &Globals) -> HashMap<FreeVar<String>, RcType> {
    use moniker::{Binder, Embed, Scope, Var};

    let universe0 = RcValue::from(Value::universe(0));
    let bool_ty = RcValue::from(Value::from(Var::Free(globals.bool.clone())));

    hashmap!{
        globals.bool.clone() => universe0.clone(),
        globals.true_.clone() => bool_ty.clone(),
        globals.false_.clone() => bool_ty.clone(),
        globals.string.clone() => universe0.clone(),
        globals.char.clone() => universe0.clone(),
        globals.u8.clone() => universe0.clone(),
        globals.u16.clone() => universe0.clone(),
        globals.u32.clone() => universe0.clone(),
        globals.u64.clone() => universe0.clone(),
        globals.i8.clone() => universe0.clone(),
        globals.i16.clone() => universe0.clone(),
        globals.i32.clone() => universe0.clone(),
        globals.i64.clone() => universe0.clone(),
        globals.f32.clone() => universe0.clone(),
        globals.f64.clone() => universe0.clone(),
        globals.array.clone() => RcValue::from(Value::Pi(Scope::new(
            (Binder(FreeVar::fresh_unnamed()), Embed(RcValue::from(Value::from(Var::Free(globals.u64.clone()))))),
            RcValue::from(Value::Pi(Scope::new(
                (Binder(FreeVar::fresh_unnamed()), Embed(universe0.clone())),
                universe0.clone(),
            ))),
        ))),
    }
}

fn default_definitions(globals: &Globals) -> HashMap<FreeVar<String>, RcTerm> {
    use syntax::core::Term;

    hashmap!{
        globals.true_.clone() => RcTerm::from(Term::Literal(Literal::Bool(true))),
        globals.false_.clone() => RcTerm::from(Term::Literal(Literal::Bool(false))),
    }
}

pub trait GlobalEnv {
    fn globals(&self) -> &Globals;
}

#[derive(Clone, Debug)]
pub struct Globals {
    pub bool: FreeVar<String>,
    pub true_: FreeVar<String>,
    pub false_: FreeVar<String>,
    pub string: FreeVar<String>,
    pub char: FreeVar<String>,
    pub u8: FreeVar<String>,
    pub u16: FreeVar<String>,
    pub u32: FreeVar<String>,
    pub u64: FreeVar<String>,
    pub i8: FreeVar<String>,
    pub i16: FreeVar<String>,
    pub i32: FreeVar<String>,
    pub i64: FreeVar<String>,
    pub f32: FreeVar<String>,
    pub f64: FreeVar<String>,
    pub array: FreeVar<String>,
}

impl Default for Globals {
    fn default() -> Globals {
        Globals {
            bool: FreeVar::fresh_named("Bool"),
            true_: FreeVar::fresh_named("true"),
            false_: FreeVar::fresh_named("false"),
            string: FreeVar::fresh_named("String"),
            char: FreeVar::fresh_named("Char"),
            u8: FreeVar::fresh_named("U8"),
            u16: FreeVar::fresh_named("U16"),
            u32: FreeVar::fresh_named("U32"),
            u64: FreeVar::fresh_named("U64"),
            i8: FreeVar::fresh_named("I8"),
            i16: FreeVar::fresh_named("I16"),
            i32: FreeVar::fresh_named("I32"),
            i64: FreeVar::fresh_named("I64"),
            f32: FreeVar::fresh_named("F32"),
            f64: FreeVar::fresh_named("F64"),
            array: FreeVar::fresh_named("Array"),
        }
    }
}

/// An environment that contains declarations
pub trait DeclarationEnv: Clone + GlobalEnv {
    fn get_declaration(&self, free_var: &FreeVar<String>) -> Option<&RcType>;
    fn insert_declaration(&mut self, free_var: FreeVar<String>, ty: RcType);
    fn extend_declarations<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (FreeVar<String>, RcType)>;
}

/// An environment that contains definitions
pub trait DefinitionEnv: Clone + GlobalEnv {
    fn get_extern_definition(&self, name: &str) -> Option<&Extern>;
    fn get_definition(&self, free_var: &FreeVar<String>) -> Option<&RcTerm>;
    fn insert_definition(&mut self, free_var: FreeVar<String>, RcTerm);
    fn extend_definitions<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (FreeVar<String>, RcTerm)>;
}

/// The type checking environment
///
/// A default environment with entries for built-in types is provided via the
/// implementation of the `Default` trait.
///
/// We use persistent data structures internally so that we can copy the
/// environment as we enter into scopes, without having to deal with the
/// error-prone tedium of working with mutable context.
#[derive(Clone, Debug)]
pub struct TcEnv {
    globals: Rc<Globals>,
    /// External definitions
    extern_definitions: HashMap<&'static str, Extern>,
    /// The type annotations of the binders we have passed over
    declarations: HashMap<FreeVar<String>, RcType>,
    /// Any definitions we have passed over
    definitions: HashMap<FreeVar<String>, RcTerm>,
}

impl TcEnv {
    pub fn mappings(&self) -> HashMap<String, FreeVar<String>> {
        self.declarations
            .iter()
            .filter_map(|(free_var, _)| {
                let pretty_name = free_var.pretty_name.as_ref()?;
                Some((pretty_name.clone(), free_var.clone()))
            }).collect()
    }
}

impl Default for TcEnv {
    fn default() -> TcEnv {
        let globals = Rc::new(Globals::default());
        let extern_definitions = default_extern_definitions();
        let declarations = default_declarations(&globals);
        let definitions = default_definitions(&globals);

        TcEnv {
            globals,
            extern_definitions,
            declarations,
            definitions,
        }
    }
}

impl GlobalEnv for TcEnv {
    fn globals(&self) -> &Globals {
        &self.globals
    }
}

impl DeclarationEnv for TcEnv {
    fn get_declaration(&self, free_var: &FreeVar<String>) -> Option<&RcType> {
        self.declarations.get(free_var)
    }

    fn insert_declaration(&mut self, free_var: FreeVar<String>, ty: RcType) {
        self.declarations.insert(free_var, ty);
    }

    fn extend_declarations<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (FreeVar<String>, RcType)>,
    {
        self.declarations.extend(iter)
    }
}

impl DefinitionEnv for TcEnv {
    fn get_extern_definition(&self, name: &str) -> Option<&Extern> {
        self.extern_definitions.get(name)
    }

    fn get_definition(&self, free_var: &FreeVar<String>) -> Option<&RcTerm> {
        self.definitions.get(free_var)
    }

    fn insert_definition(&mut self, free_var: FreeVar<String>, term: RcTerm) {
        self.definitions.insert(free_var, term);
    }

    fn extend_definitions<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = (FreeVar<String>, RcTerm)>,
    {
        self.definitions.extend(iter)
    }
}
