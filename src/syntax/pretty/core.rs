//! Pretty printing for the core syntax

use nameless::{Name, Var};
use pretty::Doc;

use syntax::core::{Constant, Context, ContextEntry, Definition, Label, Level, Module, Neutral,
                   RawConstant, RawDefinition, RawModule, RawTerm, Term, Value};

use super::{StaticDoc, ToDoc};

fn parens(doc: StaticDoc) -> StaticDoc {
    Doc::text("(").append(doc.append(Doc::text(")").nest(1)))
}

fn sexpr(name: &'static str, doc: StaticDoc) -> StaticDoc {
    parens(
        Doc::text(name)
            .append(Doc::space())
            .append(doc.nest(name.len())),
    )
}

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

fn pretty_subst<A: ToDoc, B: ToDoc>(name: &Name, ann: &A, body: &B) -> StaticDoc {
    sexpr(
        "subst",
        body.to_doc().append(Doc::space()).append(Doc::group(parens(
            Doc::text("[")
                .append(Doc::as_string(name))
                .append(Doc::space())
                .append(Doc::text("->"))
                .append(Doc::space())
                .append(ann.to_doc())
                .append(Doc::text("]")),
        ))),
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
            RawTerm::RecordType(_, ref label, ref ann, ref rest) => {
                let mut inner = Doc::nil();
                let mut label = label;
                let mut ann = ann;
                let mut rest = rest;

                loop {
                    inner = inner.append(
                        parens(Doc::as_string(&label.0))
                            .append(Doc::space())
                            .append(ann.to_doc()),
                    );

                    match **rest {
                        RawTerm::RecordType(_, ref next_label, ref next_ann, ref next_rest) => {
                            label = next_label;
                            ann = next_ann;
                            rest = next_rest;
                        },
                        RawTerm::EmptyRecordType(_) => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record_ty(inner)
            },
            RawTerm::Record(_, ref label, ref expr, ref rest) => {
                let mut inner = Doc::nil();
                let mut label = label;
                let mut expr = expr;
                let mut rest = rest;

                loop {
                    inner = inner.append(
                        parens(Doc::as_string(&label.0))
                            .append(Doc::space())
                            .append(expr.to_doc()),
                    );

                    match **rest {
                        RawTerm::Record(_, ref next_label, ref next_expr, ref next_rest) => {
                            label = next_label;
                            expr = next_expr;
                            rest = next_rest;
                        },
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
            Term::Subst(ref scope) => pretty_subst(
                &scope.unsafe_pattern.0,
                &(scope.unsafe_pattern.1).0,
                &scope.unsafe_body,
            ),
            Term::If(_, ref cond, ref if_true, ref if_false) => pretty_if(cond, if_true, if_false),
            Term::RecordType(_, ref label, ref ann, ref rest) => {
                let mut inner = Doc::nil();
                let mut label = label;
                let mut ann = ann;
                let mut rest = rest;

                loop {
                    inner = inner.append(
                        parens(Doc::as_string(&label.0))
                            .append(Doc::space())
                            .append(ann.to_doc()),
                    );

                    match **rest {
                        Term::RecordType(_, ref next_label, ref next_ann, ref next_rest) => {
                            label = next_label;
                            ann = next_ann;
                            rest = next_rest;
                        },
                        Term::EmptyRecordType(_) => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record_ty(inner)
            },
            Term::Record(_, ref label, ref expr, ref rest) => {
                let mut inner = Doc::nil();
                let mut label = label;
                let mut expr = expr;
                let mut rest = rest;

                loop {
                    inner = inner.append(
                        parens(Doc::as_string(&label.0))
                            .append(Doc::space())
                            .append(expr.to_doc()),
                    );

                    match **rest {
                        Term::Record(_, ref next_label, ref next_expr, ref next_rest) => {
                            label = next_label;
                            expr = next_expr;
                            rest = next_rest;
                        },
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
            Value::RecordType(ref label, ref ann, ref rest) => {
                let mut inner = Doc::nil();
                let mut label = label;
                let mut ann = ann;
                let mut rest = rest;

                loop {
                    inner = inner.append(
                        parens(Doc::as_string(&label.0))
                            .append(Doc::space())
                            .append(ann.to_doc()),
                    );

                    match **rest {
                        Value::RecordType(ref next_label, ref next_ann, ref next_rest) => {
                            label = next_label;
                            ann = next_ann;
                            rest = next_rest;
                        },
                        Value::EmptyRecordType => break,
                        _ => panic!("ill-formed record"),
                    }
                }

                pretty_record_ty(inner)
            },
            Value::Record(ref label, ref expr, ref rest) => {
                let mut inner = Doc::nil();
                let mut label = label;
                let mut expr = expr;
                let mut rest = rest;

                loop {
                    inner = inner.append(
                        parens(Doc::as_string(&label.0))
                            .append(Doc::space())
                            .append(expr.to_doc()),
                    );

                    match **rest {
                        Value::Record(ref next_label, ref next_expr, ref next_rest) => {
                            label = next_label;
                            expr = next_expr;
                            rest = next_rest;
                        },
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

impl ToDoc for ContextEntry {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            ContextEntry::Claim(ref name, ref ty) => sexpr(
                "claim",
                Doc::text(format!("{:#}", name))
                    .append(Doc::space())
                    .append(ty.to_doc()),
            ),
            ContextEntry::Definition(ref name, ref term) => sexpr(
                "define",
                Doc::text(format!("{:#}", name))
                    .append(Doc::space())
                    .append(term.to_doc()),
            ),
        }
    }
}

impl ToDoc for Context {
    fn to_doc(&self) -> StaticDoc {
        parens(Doc::intersperse(
            self.entries.iter().map(|entry| entry.to_doc()),
            Doc::space(),
        ))
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
            Doc::as_string(&self.name)
                .append(Doc::newline())
                .append(Doc::intersperse(
                    self.definitions
                        .iter()
                        .map(|definition| definition.to_doc()),
                    Doc::newline().append(Doc::newline()),
                )),
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
            Doc::as_string(&self.name)
                .append(Doc::newline())
                .append(Doc::intersperse(
                    self.definitions
                        .iter()
                        .map(|definition| definition.to_doc()),
                    Doc::newline().append(Doc::newline()),
                )),
        )
    }
}
