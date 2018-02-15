include!(concat!(env!("OUT_DIR"), "/syntax/parse/grammar.rs"));

/// This is an ugly hack that cobbles together a pi type from a binder term and
/// a body. See the comments on the `PiTerm` rule in the `grammer.lalrpop` for
/// more information.
fn reparse_pi_type_hack<L, T>(
    span: Span,
    binder: Term,
    body: Term,
) -> Result<Term, LalrpopError<L, T, ParseError>> {
    fn param_names<L, T>(
        term: Term,
        names: &mut Vec<(Span, String)>,
    ) -> Result<(), LalrpopError<L, T, ParseError>> {
        match term {
            Term::Var(span, name) => names.push((span, name)),
            Term::App(fn_expr, arg) => {
                param_names(*fn_expr, names)?;
                param_names(*arg, names)?;
            },
            term => {
                return Err(LalrpopError::User {
                    error: ParseError::IdentifierExpectedInPiType { span: term.span() },
                });
            },
        }
        Ok(())
    }

    match binder {
        Term::Parens(paren_span, term) => {
            let term = *term; // HACK: see https://github.com/rust-lang/rust/issues/16223
            match term {
                Term::Ann(params, ann) => {
                    let mut names = Vec::new();
                    param_names(*params, &mut names)?;
                    Ok(Term::Pi(span.lo(), (names, ann), body.into()))
                },
                ann => {
                    let parens = Term::Parens(paren_span, ann.into()).into();
                    Ok(Term::Arrow(parens, body.into()))
                },
            }
        },
        ann => Ok(Term::Arrow(ann.into(), body.into())),
    }
}

fn u32_literal<L, T>(span: Span, value: u64) -> Result<u32, LalrpopError<L, T, ParseError>> {
    if value <= u32::MAX as u64 {
        Ok(value as u32)
    } else {
        Err(LalrpopError::User {
            error: ParseError::IntegerLiteralOverflow { span, value },
        })
    }
}
