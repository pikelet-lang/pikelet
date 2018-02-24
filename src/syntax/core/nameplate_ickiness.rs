///! This is terrible, hideous, and ugly. Burn it with fire. We need to make
///! a variable binding abstraction to help us get rid of this boilerplate.
///! See the `syntax::var` module for more information!

use std::collections::HashSet;

use super::*;

impl TermLam {
    pub fn bind(param: Named<Name, Option<RcTerm>>, mut body: RcTerm) -> TermLam {
        body.close(&param.name);

        TermLam {
            unsafe_param: param,
            unsafe_body: body,
        }
    }

    pub fn unbind(mut self) -> (Named<Name, Option<RcTerm>>, RcTerm) {
        let fv = Name::fresh(self.unsafe_param.name.name());
        self.unsafe_param.name = fv.clone();
        (
            self.unsafe_param,
            self.unsafe_body
                .open(&Term::Var(SourceMeta::default(), Var::Free(fv)).into()),
        )
    }
}

impl TermPi {
    pub fn bind(param: Named<Name, RcTerm>, mut body: RcTerm) -> TermPi {
        body.close(&param.name);

        TermPi {
            unsafe_param: param,
            unsafe_body: body,
        }
    }

    pub fn unbind(mut self) -> (Named<Name, RcTerm>, RcTerm) {
        let fv = Name::fresh(self.unsafe_param.name.name());
        self.unsafe_param.name = fv.clone();
        (
            self.unsafe_param,
            self.unsafe_body
                .open(&Term::Var(SourceMeta::default(), Var::Free(fv)).into()),
        )
    }
}

impl ValueLam {
    pub fn bind(param: Named<Name, Option<RcValue>>, mut body: RcValue) -> ValueLam {
        body.close(&param.name);

        ValueLam {
            unsafe_param: param,
            unsafe_body: body,
        }
    }

    pub fn unbind(mut self) -> (Named<Name, Option<RcValue>>, RcValue) {
        let fv = Name::fresh(self.unsafe_param.name.name());
        self.unsafe_param.name = fv.clone();
        (
            self.unsafe_param,
            self.unsafe_body.open(&Value::Var(Var::Free(fv)).into()),
        )
    }
}

impl ValuePi {
    pub fn bind(param: Named<Name, RcValue>, mut body: RcValue) -> ValuePi {
        body.close(&param.name);

        ValuePi {
            unsafe_param: param,
            unsafe_body: body,
        }
    }

    pub fn unbind(mut self) -> (Named<Name, RcValue>, RcValue) {
        let fv = Name::fresh(self.unsafe_param.name.name());
        self.unsafe_param.name = fv.clone();
        (
            self.unsafe_param,
            self.unsafe_body.open(&Value::Var(Var::Free(fv)).into()),
        )
    }
}

// TODO: Would be nice for this to be more polymorphic
pub fn unbind2(
    mut lam: TermLam,
    mut pi: ValuePi,
) -> (
    Named<Name, Option<RcTerm>>,
    RcTerm,
    Named<Name, RcValue>,
    RcValue,
) {
    let fv = Name::fresh(lam.unsafe_param.name.name());
    lam.unsafe_param.name = fv.clone();
    pi.unsafe_param.name = fv.clone();
    (
        lam.unsafe_param,
        lam.unsafe_body
            .open(&Term::Var(SourceMeta::default(), Var::Free(fv.clone())).into()),
        pi.unsafe_param,
        pi.unsafe_body.open(&Value::Var(Var::Free(fv)).into()),
    )
}

impl RcTerm {
    pub fn close(&mut self, name: &Name) {
        self.close_at(Debruijn::ZERO, name);
    }

    pub fn close_at(&mut self, level: Debruijn, name: &Name) {
        *self = match *Rc::make_mut(&mut self.inner) {
            Term::Ann(_, ref mut expr, ref mut ty) => {
                expr.close_at(level, name);
                ty.close_at(level, name);
                return;
            },
            Term::Universe(_, _) => return,
            Term::Var(meta, Var::Free(ref n)) if n == name => {
                Term::Var(meta, Var::Bound(Named::new(n.clone(), level))).into()
            },
            Term::Var(_, Var::Bound(_)) | Term::Var(_, Var::Free(_)) => return,
            Term::Lam(_, ref mut lam) => {
                lam.unsafe_param
                    .inner
                    .as_mut()
                    .map(|param| param.close_at(level, name));
                lam.unsafe_body.close_at(level.succ(), name);
                return;
            },
            Term::Pi(_, ref mut pi) => {
                pi.unsafe_param.inner.close_at(level, name);
                pi.unsafe_body.close_at(level.succ(), name);
                return;
            },
            Term::App(_, ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.close_at(level, name);
                arg_expr.close_at(level, name);
                return;
            },
        };
    }

    pub fn open(&self, x: &RcTerm) -> RcTerm {
        self.open_at(Debruijn::ZERO, &x)
    }

    pub fn open_at(&self, level: Debruijn, x: &RcTerm) -> RcTerm {
        match *self.inner {
            Term::Ann(meta, ref expr, ref ty) => {
                let expr = expr.open_at(level, x);
                let ty = ty.open_at(level, x);

                Term::App(meta, expr.clone(), ty.clone()).into()
            },
            Term::Universe(_, _) => self.clone(),
            Term::Var(_, Var::Bound(Named { inner: index, .. })) if index == level => x.clone(),
            Term::Var(_, Var::Bound(_)) | Term::Var(_, Var::Free(_)) => self.clone(),
            Term::Lam(meta, ref lam) => {
                let param_ty = lam.unsafe_param
                    .inner
                    .as_ref()
                    .map(|param_ty| param_ty.open_at(level, x));
                let body = lam.unsafe_body.open_at(level.succ(), x);
                let lam = TermLam {
                    unsafe_param: Named::new(lam.unsafe_param.name.clone(), param_ty),

                    unsafe_body: body,
                };

                Term::Lam(meta, lam).into()
            },
            Term::Pi(meta, ref pi) => {
                let param_ty = pi.unsafe_param.inner.open_at(level, x);
                let body = pi.unsafe_body.open_at(level.succ(), x);
                let pi = TermPi {
                    unsafe_param: Named::new(pi.unsafe_param.name.clone(), param_ty),
                    unsafe_body: body,
                };

                Term::Pi(meta, pi).into()
            },
            Term::App(meta, ref fn_expr, ref arg_expr) => {
                let fn_expr = fn_expr.open_at(level, x);
                let arg = arg_expr.open_at(level, x);

                Term::App(meta, fn_expr, arg).into()
            },
        }
    }

    pub fn subst(&mut self, name: &Name, x: &RcTerm) {
        *self = match *Rc::make_mut(&mut self.inner) {
            Term::Ann(_, ref mut expr, ref mut ty) => {
                expr.subst(name, x);
                ty.subst(name, x);
                return;
            },
            Term::Universe(_, _) => return,
            Term::Var(_, Var::Free(ref n)) if n == name => x.clone(),
            Term::Var(_, Var::Free(_)) | Term::Var(_, Var::Bound(_)) => return,
            Term::Lam(_, ref mut lam) => {
                lam.unsafe_param
                    .inner
                    .as_mut()
                    .map(|param| param.subst(name, x));
                lam.unsafe_body.subst(name, x);
                return;
            },
            Term::Pi(_, ref mut pi) => {
                pi.unsafe_param.inner.subst(name, x);
                pi.unsafe_body.subst(name, x);
                return;
            },
            Term::App(_, ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.subst(name, x);
                arg_expr.subst(name, x);
                return;
            },
        };
    }

    fn visit_vars<F: FnMut(&Var<Name, Debruijn>)>(&self, on_var: &mut F) {
        match *self.inner {
            Term::Ann(_, ref expr, ref ty) => {
                expr.visit_vars(on_var);
                ty.visit_vars(on_var);
            },
            Term::Universe(_, _) => {},
            Term::Var(_, ref var) => on_var(var),
            Term::Lam(_, ref lam) => {
                if let Some(ref param) = lam.unsafe_param.inner {
                    param.visit_vars(on_var);
                }
                lam.unsafe_body.visit_vars(on_var);
            },
            Term::Pi(_, ref pi) => {
                pi.unsafe_param.inner.visit_vars(on_var);
                pi.unsafe_body.visit_vars(on_var);
            },
            Term::App(_, ref fn_expr, ref arg_expr) => {
                fn_expr.visit_vars(on_var);
                arg_expr.visit_vars(on_var);
            },
        };
    }

    pub fn free_vars(&self) -> HashSet<Name> {
        let mut free_vars = HashSet::new();
        self.visit_vars(&mut |var| match *var {
            Var::Bound(_) => {},
            Var::Free(ref name) => {
                free_vars.insert(name.clone());
            },
        });
        free_vars
    }
}

impl RcValue {
    pub fn close(&mut self, name: &Name) {
        self.close_at(Debruijn::ZERO, name);
    }

    pub fn close_at(&mut self, level: Debruijn, name: &Name) {
        *self = match *Rc::make_mut(&mut self.inner) {
            Value::Universe(_) => return,
            Value::Var(Var::Free(ref n)) if n == name => {
                Value::Var(Var::Bound(Named::new(n.clone(), level))).into()
            },
            Value::Var(Var::Bound(_)) | Value::Var(Var::Free(_)) => return,
            Value::Lam(ref mut lam) => {
                lam.unsafe_param
                    .inner
                    .as_mut()
                    .map(|param| param.close_at(level, name));
                lam.unsafe_body.close_at(level.succ(), name);
                return;
            },
            Value::Pi(ref mut pi) => {
                pi.unsafe_param.inner.close_at(level, name);
                pi.unsafe_body.close_at(level.succ(), name);
                return;
            },
            Value::App(ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.close_at(level, name);
                arg_expr.close_at(level, name);
                return;
            },
        };
    }

    pub fn open(&self, x: &RcValue) -> RcValue {
        self.open_at(Debruijn::ZERO, &x)
    }

    pub fn open_at(&self, level: Debruijn, x: &RcValue) -> RcValue {
        match *self.inner {
            Value::Universe(_) => self.clone(),
            Value::Var(Var::Bound(Named { inner: index, .. })) if index == level => x.clone(),
            Value::Var(Var::Bound(_)) | Value::Var(Var::Free(_)) => self.clone(),
            Value::Lam(ref lam) => {
                let param_ty = lam.unsafe_param
                    .inner
                    .as_ref()
                    .map(|param_ty| param_ty.open_at(level, x));
                let body = lam.unsafe_body.open_at(level.succ(), x);

                Value::Lam(ValueLam {
                    unsafe_param: Named::new(lam.unsafe_param.name.clone(), param_ty),
                    unsafe_body: body,
                }).into()
            },
            Value::Pi(ref pi) => {
                let param_ty = pi.unsafe_param.inner.open_at(level, x);
                let body = pi.unsafe_body.open_at(level.succ(), x);

                Value::Pi(ValuePi {
                    unsafe_param: Named::new(pi.unsafe_param.name.clone(), param_ty),
                    unsafe_body: body,
                }).into()
            },
            Value::App(ref fn_expr, ref arg_expr) => {
                let fn_expr = fn_expr.open_at(level, x);
                let arg = arg_expr.open_at(level, x);

                Value::App(fn_expr, arg).into()
            },
        }
    }

    pub fn subst(&mut self, name: &Name, x: &RcValue) {
        *self = match *Rc::make_mut(&mut self.inner) {
            Value::Universe(_) => return,
            Value::Var(Var::Free(ref n)) if n == name => x.clone(),
            Value::Var(Var::Free(_)) | Value::Var(Var::Bound(_)) => return,
            Value::Lam(ref mut lam) => {
                lam.unsafe_param
                    .inner
                    .as_mut()
                    .map(|param| param.subst(name, x));
                lam.unsafe_body.subst(name, x);
                return;
            },
            Value::Pi(ref mut pi) => {
                pi.unsafe_param.inner.subst(name, x);
                pi.unsafe_body.subst(name, x);
                return;
            },
            Value::App(ref mut fn_expr, ref mut arg_expr) => {
                fn_expr.subst(name, x);
                arg_expr.subst(name, x);
                return;
            },
        };
    }
}
