use im::Vector;
use moniker::FreeVar;
use std::fmt;
use std::rc::Rc;

use syntax::core::{RcTerm, RcType, Term};
use syntax::pretty::{self, ToDoc};
use syntax::prim::{self, PrimFn};

/// An entry in the context
#[derive(Debug, Clone)]
pub enum Entry {
    /// A type claim
    Claim(FreeVar<String>, RcType),
    /// A value definition
    Definition(FreeVar<String>, Definition),
}

#[derive(Debug, Clone)]
pub enum Definition {
    Term(RcTerm),
    Prim(Rc<PrimFn>),
}

/// A list of binders that have been accumulated during type checking
///
/// We use a persistent vector internally so that we don't need to deal with the
/// error-prone tedium of dealing with a mutable context when entering and
/// exiting binders.
#[derive(Clone)]
pub struct Context {
    pub entries: Vector<Entry>,
}

impl Context {
    /// Create a new, empty context
    pub fn new() -> Context {
        Context {
            entries: Vector::new(),
        }
    }

    pub fn claim(&self, name: FreeVar<String>, ann: RcType) -> Context {
        Context {
            entries: self.entries.push_front(Entry::Claim(name, ann)),
        }
    }

    pub fn define_term(&self, name: FreeVar<String>, ann: RcType, term: RcTerm) -> Context {
        Context {
            entries: self
                .entries
                .push_front(Entry::Claim(name.clone(), ann))
                .push_front(Entry::Definition(name, Definition::Term(term))),
        }
    }

    fn define_prim(&self, name: FreeVar<String>, prim: Rc<PrimFn>) -> Context {
        Context {
            entries: self
                .entries
                .push_front(Entry::Claim(name.clone(), prim.ann.clone()))
                .push_front(Entry::Definition(name, Definition::Prim(prim))),
        }
    }

    pub fn lookup_claim(&self, name: &FreeVar<String>) -> Option<RcType> {
        self.entries
            .iter()
            .filter_map(|entry| match *entry {
                Entry::Claim(ref n, ref ty) if n == name => Some(ty.clone()),
                Entry::Claim(_, _) | Entry::Definition(_, _) => None,
            })
            .next()
    }

    pub fn lookup_definition(&self, name: &FreeVar<String>) -> Option<Definition> {
        self.entries
            .iter()
            .filter_map(|entry| match *entry {
                Entry::Definition(ref n, ref def) if n == name => Some(def.clone()),
                Entry::Definition(_, _) | Entry::Claim(_, _) => None,
            })
            .next()
    }
}

impl Default for Context {
    fn default() -> Context {
        use moniker::{Embed, GenId, Scope, Var};

        use syntax::core::{Literal, RcValue, Value};
        use syntax::Level;

        let name = FreeVar::user;
        let fresh_name = || FreeVar::from(GenId::fresh());
        let free_var = |n| RcValue::from(Value::from(Var::Free(name(n))));
        let universe0 = RcValue::from(Value::Universe(Level(0)));
        let bool_lit = |val| RcTerm::from(Term::Literal(Literal::Bool(val)));

        Context::new()
            .claim(name("Bool"), universe0.clone())
            .define_term(name("true"), free_var("Bool"), bool_lit(true))
            .define_term(name("false"), free_var("Bool"), bool_lit(false))
            .claim(name("String"), universe0.clone())
            .claim(name("Char"), universe0.clone())
            .claim(name("U8"), universe0.clone())
            .claim(name("U16"), universe0.clone())
            .claim(name("U32"), universe0.clone())
            .claim(name("U64"), universe0.clone())
            .claim(name("I8"), universe0.clone())
            .claim(name("I16"), universe0.clone())
            .claim(name("I32"), universe0.clone())
            .claim(name("I64"), universe0.clone())
            .claim(name("F32"), universe0.clone())
            .claim(name("F64"), universe0.clone())
            .claim(
                name("Array"),
                RcValue::from(Value::Pi(Scope::new(
                    (fresh_name(), Embed(free_var("U64"))),
                    RcValue::from(Value::Pi(Scope::new(
                        (fresh_name(), Embed(universe0.clone())),
                        universe0.clone(),
                    ))),
                ))),
            )
            .define_prim(name("prim-string-eq"), Rc::new(prim::string_eq()))
            .define_prim(name("prim-bool-eq"), Rc::new(prim::bool_eq()))
            .define_prim(name("prim-char-eq"), Rc::new(prim::char_eq()))
            .define_prim(name("prim-u8-eq"), Rc::new(prim::u8_eq()))
            .define_prim(name("prim-u16-eq"), Rc::new(prim::u16_eq()))
            .define_prim(name("prim-u32-eq"), Rc::new(prim::u32_eq()))
            .define_prim(name("prim-u64-eq"), Rc::new(prim::u64_eq()))
            .define_prim(name("prim-i8-eq"), Rc::new(prim::i8_eq()))
            .define_prim(name("prim-i16-eq"), Rc::new(prim::i16_eq()))
            .define_prim(name("prim-i32-eq"), Rc::new(prim::i32_eq()))
            .define_prim(name("prim-i64-eq"), Rc::new(prim::i64_eq()))
            .define_prim(name("prim-f32-eq"), Rc::new(prim::f32_eq()))
            .define_prim(name("prim-f64-eq"), Rc::new(prim::f64_eq()))
            .define_prim(name("prim-string-ne"), Rc::new(prim::string_ne()))
            .define_prim(name("prim-bool-ne"), Rc::new(prim::bool_ne()))
            .define_prim(name("prim-char-ne"), Rc::new(prim::char_ne()))
            .define_prim(name("prim-u8-ne"), Rc::new(prim::u8_ne()))
            .define_prim(name("prim-u16-ne"), Rc::new(prim::u16_ne()))
            .define_prim(name("prim-u32-ne"), Rc::new(prim::u32_ne()))
            .define_prim(name("prim-u64-ne"), Rc::new(prim::u64_ne()))
            .define_prim(name("prim-i8-ne"), Rc::new(prim::i8_ne()))
            .define_prim(name("prim-i16-ne"), Rc::new(prim::i16_ne()))
            .define_prim(name("prim-i32-ne"), Rc::new(prim::i32_ne()))
            .define_prim(name("prim-i64-ne"), Rc::new(prim::i64_ne()))
            .define_prim(name("prim-f32-ne"), Rc::new(prim::f32_ne()))
            .define_prim(name("prim-f64-ne"), Rc::new(prim::f64_ne()))
            .define_prim(name("prim-string-le"), Rc::new(prim::string_le()))
            .define_prim(name("prim-bool-le"), Rc::new(prim::bool_le()))
            .define_prim(name("prim-char-le"), Rc::new(prim::char_le()))
            .define_prim(name("prim-u8-le"), Rc::new(prim::u8_le()))
            .define_prim(name("prim-u16-le"), Rc::new(prim::u16_le()))
            .define_prim(name("prim-u32-le"), Rc::new(prim::u32_le()))
            .define_prim(name("prim-u64-le"), Rc::new(prim::u64_le()))
            .define_prim(name("prim-i8-le"), Rc::new(prim::i8_le()))
            .define_prim(name("prim-i16-le"), Rc::new(prim::i16_le()))
            .define_prim(name("prim-i32-le"), Rc::new(prim::i32_le()))
            .define_prim(name("prim-i64-le"), Rc::new(prim::i64_le()))
            .define_prim(name("prim-f32-le"), Rc::new(prim::f32_le()))
            .define_prim(name("prim-f64-le"), Rc::new(prim::f64_le()))
            .define_prim(name("prim-string-lt"), Rc::new(prim::string_lt()))
            .define_prim(name("prim-bool-lt"), Rc::new(prim::bool_lt()))
            .define_prim(name("prim-char-lt"), Rc::new(prim::char_lt()))
            .define_prim(name("prim-u8-lt"), Rc::new(prim::u8_lt()))
            .define_prim(name("prim-u16-lt"), Rc::new(prim::u16_lt()))
            .define_prim(name("prim-u32-lt"), Rc::new(prim::u32_lt()))
            .define_prim(name("prim-u64-lt"), Rc::new(prim::u64_lt()))
            .define_prim(name("prim-i8-lt"), Rc::new(prim::i8_lt()))
            .define_prim(name("prim-i16-lt"), Rc::new(prim::i16_lt()))
            .define_prim(name("prim-i32-lt"), Rc::new(prim::i32_lt()))
            .define_prim(name("prim-i64-lt"), Rc::new(prim::i64_lt()))
            .define_prim(name("prim-f32-lt"), Rc::new(prim::f32_lt()))
            .define_prim(name("prim-f64-lt"), Rc::new(prim::f64_lt()))
            .define_prim(name("prim-string-gt"), Rc::new(prim::string_gt()))
            .define_prim(name("prim-bool-gt"), Rc::new(prim::bool_gt()))
            .define_prim(name("prim-char-gt"), Rc::new(prim::char_gt()))
            .define_prim(name("prim-u8-gt"), Rc::new(prim::u8_gt()))
            .define_prim(name("prim-u16-gt"), Rc::new(prim::u16_gt()))
            .define_prim(name("prim-u32-gt"), Rc::new(prim::u32_gt()))
            .define_prim(name("prim-u64-gt"), Rc::new(prim::u64_gt()))
            .define_prim(name("prim-i8-gt"), Rc::new(prim::i8_gt()))
            .define_prim(name("prim-i16-gt"), Rc::new(prim::i16_gt()))
            .define_prim(name("prim-i32-gt"), Rc::new(prim::i32_gt()))
            .define_prim(name("prim-i64-gt"), Rc::new(prim::i64_gt()))
            .define_prim(name("prim-f32-gt"), Rc::new(prim::f32_gt()))
            .define_prim(name("prim-f64-gt"), Rc::new(prim::f64_gt()))
            .define_prim(name("prim-string-ge"), Rc::new(prim::string_ge()))
            .define_prim(name("prim-bool-ge"), Rc::new(prim::bool_ge()))
            .define_prim(name("prim-char-ge"), Rc::new(prim::char_ge()))
            .define_prim(name("prim-u8-ge"), Rc::new(prim::u8_ge()))
            .define_prim(name("prim-u16-ge"), Rc::new(prim::u16_ge()))
            .define_prim(name("prim-u32-ge"), Rc::new(prim::u32_ge()))
            .define_prim(name("prim-u64-ge"), Rc::new(prim::u64_ge()))
            .define_prim(name("prim-i8-ge"), Rc::new(prim::i8_ge()))
            .define_prim(name("prim-i16-ge"), Rc::new(prim::i16_ge()))
            .define_prim(name("prim-i32-ge"), Rc::new(prim::i32_ge()))
            .define_prim(name("prim-i64-ge"), Rc::new(prim::i64_ge()))
            .define_prim(name("prim-f32-ge"), Rc::new(prim::f32_ge()))
            .define_prim(name("prim-f64-ge"), Rc::new(prim::f64_ge()))
            .define_prim(name("prim-u8-add"), Rc::new(prim::u8_add()))
            .define_prim(name("prim-u16-add"), Rc::new(prim::u16_add()))
            .define_prim(name("prim-u32-add"), Rc::new(prim::u32_add()))
            .define_prim(name("prim-u64-add"), Rc::new(prim::u64_add()))
            .define_prim(name("prim-i8-add"), Rc::new(prim::i8_add()))
            .define_prim(name("prim-i16-add"), Rc::new(prim::i16_add()))
            .define_prim(name("prim-i32-add"), Rc::new(prim::i32_add()))
            .define_prim(name("prim-i64-add"), Rc::new(prim::i64_add()))
            .define_prim(name("prim-f32-add"), Rc::new(prim::f32_add()))
            .define_prim(name("prim-f64-add"), Rc::new(prim::f64_add()))
            .define_prim(name("prim-u8-sub"), Rc::new(prim::u8_sub()))
            .define_prim(name("prim-u16-sub"), Rc::new(prim::u16_sub()))
            .define_prim(name("prim-u32-sub"), Rc::new(prim::u32_sub()))
            .define_prim(name("prim-u64-sub"), Rc::new(prim::u64_sub()))
            .define_prim(name("prim-i8-sub"), Rc::new(prim::i8_sub()))
            .define_prim(name("prim-i16-sub"), Rc::new(prim::i16_sub()))
            .define_prim(name("prim-i32-sub"), Rc::new(prim::i32_sub()))
            .define_prim(name("prim-i64-sub"), Rc::new(prim::i64_sub()))
            .define_prim(name("prim-f32-sub"), Rc::new(prim::f32_sub()))
            .define_prim(name("prim-f64-sub"), Rc::new(prim::f64_sub()))
            .define_prim(name("prim-u8-mul"), Rc::new(prim::u8_mul()))
            .define_prim(name("prim-u16-mul"), Rc::new(prim::u16_mul()))
            .define_prim(name("prim-u32-mul"), Rc::new(prim::u32_mul()))
            .define_prim(name("prim-u64-mul"), Rc::new(prim::u64_mul()))
            .define_prim(name("prim-i8-mul"), Rc::new(prim::i8_mul()))
            .define_prim(name("prim-i16-mul"), Rc::new(prim::i16_mul()))
            .define_prim(name("prim-i32-mul"), Rc::new(prim::i32_mul()))
            .define_prim(name("prim-i64-mul"), Rc::new(prim::i64_mul()))
            .define_prim(name("prim-f32-mul"), Rc::new(prim::f32_mul()))
            .define_prim(name("prim-f64-mul"), Rc::new(prim::f64_mul()))
            .define_prim(name("prim-u8-div"), Rc::new(prim::u8_div()))
            .define_prim(name("prim-u16-div"), Rc::new(prim::u16_div()))
            .define_prim(name("prim-u32-div"), Rc::new(prim::u32_div()))
            .define_prim(name("prim-u64-div"), Rc::new(prim::u64_div()))
            .define_prim(name("prim-i8-div"), Rc::new(prim::i8_div()))
            .define_prim(name("prim-i16-div"), Rc::new(prim::i16_div()))
            .define_prim(name("prim-i32-div"), Rc::new(prim::i32_div()))
            .define_prim(name("prim-i64-div"), Rc::new(prim::i64_div()))
            .define_prim(name("prim-f32-div"), Rc::new(prim::f32_div()))
            .define_prim(name("prim-f64-div"), Rc::new(prim::f64_div()))
            .define_prim(name("prim-char-to-string"), Rc::new(prim::char_to_string()))
            .define_prim(name("prim-u8-to-string"), Rc::new(prim::u8_to_string()))
            .define_prim(name("prim-u16-to-string"), Rc::new(prim::u16_to_string()))
            .define_prim(name("prim-u32-to-string"), Rc::new(prim::u32_to_string()))
            .define_prim(name("prim-u64-to-string"), Rc::new(prim::u64_to_string()))
            .define_prim(name("prim-i8-to-string"), Rc::new(prim::i8_to_string()))
            .define_prim(name("prim-i16-to-string"), Rc::new(prim::i16_to_string()))
            .define_prim(name("prim-i32-to-string"), Rc::new(prim::i32_to_string()))
            .define_prim(name("prim-i64-to-string"), Rc::new(prim::i64_to_string()))
            .define_prim(name("prim-f32-to-string"), Rc::new(prim::f32_to_string()))
            .define_prim(name("prim-f64-to-string"), Rc::new(prim::f64_to_string()))
            .define_prim(name("prim-string-append"), Rc::new(prim::string_append()))
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.to_doc().group().render_fmt(pretty::FALLBACK_WIDTH, f)
    }
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct FmtContextEntries<'a>(&'a Vector<Entry>);

        impl<'a> fmt::Debug for FmtContextEntries<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_list().entries(self.0).finish()
            }
        }

        f.debug_struct("Context")
            .field("entries", &FmtContextEntries(&self.entries))
            .finish()
    }
}
