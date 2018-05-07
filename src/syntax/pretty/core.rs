//! Pretty printing for the core syntax

use nameless::{Name, Var};
use pretty::Doc;

use syntax::core::{
    Constant, Definition, Label, Level, Module, Neutral, RawConstant, RawDefinition, RawModule,
    RawTerm, Term, Value,
};

use super::{parens, sexpr, StaticDoc, ToDoc};

fn pretty_ann<E: ToDoc, T: ToDoc>(expr: &E, ty: &T) -> StaticDoc {
    sexpr(
        "ann",
        expr.to_doc().append(Doc::space()).append(ty.to_doc()),
    )
}

fn pretty_universe(level: Level) -> StaticDoc {
    sexpr("Type", Doc::as_string(level))
}

fn pretty_var(var: &Var) -> StaticDoc {
    sexpr("var", Doc::text(format!("{:#}", var)))
}

fn pretty_lam<A: ToDoc, B: ToDoc>(name: &Name, ann: &A, body: &B) -> StaticDoc {
    sexpr(
        "λ",
        Doc::group(parens(
            Doc::as_string(name)
                .append(Doc::space())
                .append(ann.to_doc().group()),
        )).append(Doc::space())
            .append(body.to_doc()),
    )
}

fn pretty_pi<A: ToDoc, B: ToDoc>(name: &Name, ann: &A, body: &B) -> StaticDoc {
    sexpr(
        "Π",
        Doc::group(parens(
            Doc::as_string(name)
                .append(Doc::space())
                .append(ann.to_doc().group()),
        )).append(Doc::space())
            .append(body.to_doc()),
    )
}

fn pretty_app<F: ToDoc, A: ToDoc>(fn_term: &F, arg_term: &A) -> StaticDoc {
    sexpr(
        "app",
        Doc::nil()
            .append(fn_term.to_doc())
            .append(Doc::space())
            .append(arg_term.to_doc()),
    )
}

fn pretty_if<C: ToDoc, T: ToDoc, F: ToDoc>(cond: &C, if_true: &T, if_false: &F) -> StaticDoc {
    sexpr(
        "if",
        cond.to_doc()
            .append(Doc::space())
            .append(if_true.to_doc())
            .append(Doc::space())
            .append(if_false.to_doc()),
    )
}

fn pretty_record_ty(inner: StaticDoc) -> StaticDoc {
    sexpr("Record", inner)
}

fn pretty_record(inner: StaticDoc) -> StaticDoc {
    sexpr("record", inner)
}

fn pretty_empty_record_ty() -> StaticDoc {
    pretty_record_ty(Doc::text("()"))
}

fn pretty_empty_record() -> StaticDoc {
    pretty_record(Doc::text("()"))
}

fn pretty_proj<E: ToDoc>(expr: &E, label: &Label) -> StaticDoc {
    sexpr(
        "proj",
        expr.to_doc()
            .append(Doc::space())
            .append(Doc::as_string(&label.0)),
    )
}

impl ToDoc for RawConstant {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            RawConstant::String(ref value) => Doc::text(format!("{:?}", value)),
            RawConstant::Char(value) => Doc::text(format!("{:?}", value)),
            RawConstant::Int(value) => Doc::as_string(value),
            RawConstant::Float(value) => Doc::as_string(value),
        }
    }
}

impl ToDoc for Constant {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Constant::Bool(true) => Doc::text("#true"),
            Constant::Bool(false) => Doc::text("#false"),
            Constant::String(ref value) => Doc::text(format!("{:?}", value)),
            Constant::Char(value) => Doc::text(format!("{:?}", value)),
            Constant::U8(value) => Doc::as_string(value),
            Constant::U16(value) => Doc::as_string(value),
            Constant::U32(value) => Doc::as_string(value),
            Constant::U64(value) => Doc::as_string(value),
            Constant::I8(value) => Doc::as_string(value),
            Constant::I16(value) => Doc::as_string(value),
            Constant::I32(value) => Doc::as_string(value),
            Constant::I64(value) => Doc::as_string(value),
            Constant::F32(value) => Doc::as_string(value),
            Constant::F64(value) => Doc::as_string(value),
            Constant::BoolType => Doc::text("#Bool"),
            Constant::StringType => Doc::text("#String"),
            Constant::CharType => Doc::text("#Char"),
            Constant::U8Type => Doc::text("#U8"),
            Constant::U16Type => Doc::text("#U16"),
            Constant::U32Type => Doc::text("#U32"),
            Constant::U64Type => Doc::text("#U64"),
            Constant::I8Type => Doc::text("#I8"),
            Constant::I16Type => Doc::text("#I16"),
            Constant::I32Type => Doc::text("#I32"),
            Constant::I64Type => Doc::text("#I64"),
            Constant::F32Type => Doc::text("#F32"),
            Constant::F64Type => Doc::text("#F64"),
        }
    }
}

impl ToDoc for RawTerm {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            RawTerm::Ann(_, ref expr, ref ty) => pretty_ann(expr, ty),
            RawTerm::Universe(_, level) => pretty_universe(level),
            RawTerm::Hole(_) => parens(Doc::text("hole")),
            RawTerm::Constant(_, ref c) => c.to_doc(),
            RawTerm::Var(_, ref var) => pretty_var(var),
            RawTerm::Lam(_, ref scope) => pretty_lam(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0,
                &scope.unsafe_body,
            ),
            RawTerm::Pi(_, ref scope) => pretty_pi(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0,
                &scope.unsafe_body,
            ),
            RawTerm::App(ref f, ref a) => pretty_app(f, a),
            RawTerm::If(_, ref cond, ref if_true, ref if_false) => {
                pretty_if(cond, if_true, if_false)
            },
            RawTerm::RecordType(_, ref scope) => {
                let mut inner = Doc::nil();
                let mut scope = scope;

                for i in 0.. {
                    inner = inner
                        .append(match i {
                            0 => Doc::nil(),
                            _ => Doc::space(),
                        })
                        .append(parens(
                            Doc::as_string(&(scope.unsafe_pattern.0).0)
                                .append(Doc::space())
                                .append((scope.unsafe_pattern.1).0.to_doc()),
                        ));

                    match *scope.unsafe_body {
                        RawTerm::RecordType(_, ref next_scope) => scope = next_scope,
                        RawTerm::EmptyRecordType(_) => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record_ty(inner)
            },
            RawTerm::Record(_, ref scope) => {
                let mut inner = Doc::nil();
                let mut scope = scope;

                for i in 0.. {
                    inner = inner
                        .append(match i {
                            0 => Doc::nil(),
                            _ => Doc::space(),
                        })
                        .append(parens(
                            Doc::as_string(&(scope.unsafe_pattern.0).0)
                                .append(Doc::space())
                                .append((scope.unsafe_pattern.1).0.to_doc()),
                        ));

                    match *scope.unsafe_body {
                        RawTerm::Record(_, ref next_scope) => scope = next_scope,
                        RawTerm::EmptyRecord(_) => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record(inner)
            },
            RawTerm::EmptyRecordType(_) => pretty_empty_record_ty(),
            RawTerm::EmptyRecord(_) => pretty_empty_record(),
            RawTerm::Proj(_, ref expr, _, ref label) => pretty_proj(expr, label),
        }
    }
}

impl ToDoc for Term {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Term::Ann(_, ref expr, ref ty) => pretty_ann(expr, ty),
            Term::Universe(_, level) => pretty_universe(level),
            Term::Constant(_, ref c) => c.to_doc(),
            Term::Var(_, ref var) => pretty_var(var),
            Term::Lam(_, ref scope) => pretty_lam(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0,
                &scope.unsafe_body,
            ),
            Term::Pi(_, ref scope) => pretty_pi(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0,
                &scope.unsafe_body,
            ),
            Term::App(ref f, ref a) => pretty_app(f, a),
            Term::If(_, ref cond, ref if_true, ref if_false) => pretty_if(cond, if_true, if_false),
            Term::RecordType(_, ref scope) => {
                let mut inner = Doc::nil();
                let mut scope = scope;

                for i in 0.. {
                    inner = inner
                        .append(match i {
                            0 => Doc::nil(),
                            _ => Doc::space(),
                        })
                        .append(parens(
                            Doc::as_string(&(scope.unsafe_pattern.0).0)
                                .append(Doc::space())
                                .append((scope.unsafe_pattern.1).0.to_doc()),
                        ));

                    match *scope.unsafe_body {
                        Term::RecordType(_, ref next_scope) => scope = next_scope,
                        Term::EmptyRecordType(_) => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record_ty(inner)
            },
            Term::Record(_, ref scope) => {
                let mut inner = Doc::nil();
                let mut scope = scope;

                for i in 0.. {
                    inner = inner
                        .append(match i {
                            0 => Doc::nil(),
                            _ => Doc::space(),
                        })
                        .append(parens(
                            Doc::as_string(&(scope.unsafe_pattern.0).0)
                                .append(Doc::space())
                                .append((scope.unsafe_pattern.1).0.to_doc()),
                        ));

                    match *scope.unsafe_body {
                        Term::Record(_, ref next_scope) => scope = next_scope,
                        Term::EmptyRecord(_) => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record(inner)
            },
            Term::EmptyRecordType(_) => pretty_empty_record_ty(),
            Term::EmptyRecord(_) => pretty_empty_record(),
            Term::Proj(_, ref expr, _, ref label) => pretty_proj(expr, label),
        }
    }
}

impl ToDoc for Value {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Value::Universe(level) => pretty_universe(level),
            Value::Constant(ref c) => c.to_doc(),
            Value::Lam(ref scope) => pretty_lam(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0,
                &scope.unsafe_body,
            ),
            Value::Pi(ref scope) => pretty_pi(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0,
                &scope.unsafe_body,
            ),
            Value::RecordType(ref scope) => {
                let mut inner = Doc::nil();
                let mut scope = scope;

                for i in 0.. {
                    inner = inner
                        .append(match i {
                            0 => Doc::nil(),
                            _ => Doc::space(),
                        })
                        .append(parens(
                            Doc::as_string(&(scope.unsafe_pattern.0).0)
                                .append(Doc::space())
                                .append((scope.unsafe_pattern.1).0.to_doc()),
                        ));

                    match *scope.unsafe_body {
                        Value::RecordType(ref next_scope) => scope = next_scope,
                        Value::EmptyRecordType => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record_ty(inner)
            },
            Value::Record(ref scope) => {
                let mut inner = Doc::nil();
                let mut scope = scope;

                for i in 0.. {
                    inner = inner
                        .append(match i {
                            0 => Doc::nil(),
                            _ => Doc::space(),
                        })
                        .append(parens(
                            Doc::as_string(&(scope.unsafe_pattern.0).0)
                                .append(Doc::space())
                                .append((scope.unsafe_pattern.1).0.to_doc()),
                        ));

                    match *scope.unsafe_body {
                        Value::Record(ref next_scope) => scope = next_scope,
                        Value::EmptyRecord => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record(inner)
            },
            Value::EmptyRecordType => pretty_empty_record_ty(),
            Value::EmptyRecord => pretty_empty_record(),
            Value::Neutral(ref n) => n.to_doc(),
        }
    }
}

impl ToDoc for Neutral {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Neutral::Var(ref var) => pretty_var(var),
            Neutral::App(ref fn_term, ref arg_term) => pretty_app(fn_term, arg_term),
            Neutral::If(ref cond, ref if_true, ref if_false) => pretty_if(cond, if_true, if_false),
            Neutral::Proj(ref expr, ref label) => pretty_proj(expr, label),
        }
    }
}

impl ToDoc for RawDefinition {
    fn to_doc(&self) -> StaticDoc {
        sexpr(
            "define",
            Doc::as_string(&self.name)
                .append(Doc::space())
                .append(self.ann.to_doc())
                .append(Doc::space())
                .append(self.term.to_doc()),
        )
    }
}

impl ToDoc for RawModule {
    fn to_doc(&self) -> StaticDoc {
        sexpr(
            "module",
            Doc::intersperse(
                self.definitions
                    .iter()
                    .map(|definition| definition.to_doc()),
                Doc::newline().append(Doc::newline()),
            ),
        )
    }
}

impl ToDoc for Definition {
    fn to_doc(&self) -> StaticDoc {
        sexpr(
            "define",
            Doc::as_string(&self.name)
                .append(Doc::space())
                .append(self.ann.to_doc())
                .append(Doc::space())
                .append(self.term.to_doc()),
        )
    }
}

impl ToDoc for Module {
    fn to_doc(&self) -> StaticDoc {
        sexpr(
            "module",
            Doc::intersperse(
                self.definitions
                    .iter()
                    .map(|definition| definition.to_doc()),
                Doc::newline().append(Doc::newline()),
            ),
        )
    }
}
