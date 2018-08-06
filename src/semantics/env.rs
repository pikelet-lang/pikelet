use im::HashMap;
use moniker::FreeVar;

use semantics::PrimEnv;
use syntax::core::{RcTerm, RcType, RcValue};

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
    /// Primitive definitions
    pub primitives: PrimEnv,
    /// Global annotation/definition pairs
    pub globals: HashMap<&'static str, (Option<RcValue>, RcType)>,
    /// The type annotations of the binders we have passed over
    pub claims: HashMap<FreeVar<String>, RcType>,
    /// Any definitions we have passed over
    pub definitions: HashMap<FreeVar<String>, RcTerm>,
}

impl Default for TcEnv {
    fn default() -> TcEnv {
        use moniker::{Binder, Embed, Scope};

        use syntax::core::{Literal, Value};

        let universe0 = RcValue::from(Value::universe(0));
        let true_value = RcValue::from(Value::Literal(Literal::Bool(true)));
        let false_value = RcValue::from(Value::Literal(Literal::Bool(false)));
        let bool_ty = RcValue::from(Value::global("Bool"));

        TcEnv {
            primitives: PrimEnv::default(),
            globals: hashmap!{
                "Bool" => (None, universe0.clone()),
                "true" => (Some(true_value), bool_ty.clone()),
                "false" => (Some(false_value), bool_ty.clone()),
                "String" => (None, universe0.clone()),
                "Char" => (None, universe0.clone()),
                "U8" => (None, universe0.clone()),
                "U16" => (None, universe0.clone()),
                "U32" => (None, universe0.clone()),
                "U64" => (None, universe0.clone()),
                "I8" => (None, universe0.clone()),
                "I16" => (None, universe0.clone()),
                "I32" => (None, universe0.clone()),
                "I64" => (None, universe0.clone()),
                "F32" => (None, universe0.clone()),
                "F64" => (None, universe0.clone()),
                "Array" => (None, RcValue::from(Value::Pi(Scope::new(
                    (Binder(FreeVar::fresh_unnamed()), Embed(RcValue::from(Value::global("U64")))),
                    RcValue::from(Value::Pi(Scope::new(
                        (Binder(FreeVar::fresh_unnamed()), Embed(universe0.clone())),
                        universe0.clone(),
                    ))),
                )))),
            },
            claims: hashmap!{},
            definitions: hashmap!{},
        }
    }
}
