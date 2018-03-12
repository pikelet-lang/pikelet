use {AlphaEq, Debruijn, FreeName, LocallyNameless, Named};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope<B, T> {
    pub unsafe_binder: B,
    pub unsafe_body: T,
}

impl<N: FreeName, B, T> Scope<Named<N, B>, T>
where
    B: LocallyNameless<Name = N>,
    T: LocallyNameless<Name = N>,
{
    pub fn bind(binder: Named<N, B>, mut body: T) -> Scope<Named<N, B>, T> {
        body.close(&binder.name);

        Scope {
            unsafe_binder: binder,
            unsafe_body: body,
        }
    }

    pub fn unbind(self) -> (Named<N, B>, T) {
        let mut binder = self.unsafe_binder;
        let mut body = self.unsafe_body;

        binder.name.freshen();
        body.open(&binder.name);

        (binder, body)
    }
}

impl<B: AlphaEq, T: AlphaEq> AlphaEq for Scope<B, T> {
    fn alpha_eq(&self, other: &Scope<B, T>) -> bool {
        B::alpha_eq(&self.unsafe_binder, &other.unsafe_binder)
            && T::alpha_eq(&self.unsafe_body, &other.unsafe_body)
    }
}

impl<N: FreeName, B, T> LocallyNameless for Scope<Named<N, B>, T>
where
    B: LocallyNameless<Name = N>,
    T: LocallyNameless<Name = N>,
{
    type Name = N;

    fn close_at(&mut self, index: Debruijn, name: &N) {
        self.unsafe_binder.close_at(index, name);
        self.unsafe_body.close_at(index.succ(), name);
    }

    fn open_at(&mut self, index: Debruijn, name: &N) {
        self.unsafe_binder.open_at(index, &name);
        self.unsafe_body.open_at(index.succ(), &name);
    }
}

pub fn unbind2<N, B1, T1, B2, T2>(
    scope1: Scope<Named<N, B1>, T1>,
    scope2: Scope<Named<N, B2>, T2>,
) -> (Named<N, B1>, T1, Named<N, B2>, T2)
where
    N: FreeName,
    B1: LocallyNameless<Name = N>,
    T1: LocallyNameless<Name = N>,
    B2: LocallyNameless<Name = N>,
    T2: LocallyNameless<Name = N>,
{
    let mut scope1_binder = scope1.unsafe_binder;
    let mut scope1_body = scope1.unsafe_body;
    let mut scope2_binder = scope2.unsafe_binder;
    let mut scope2_body = scope2.unsafe_body;

    scope1_binder.name.freshen();
    scope1_body.open(&scope1_binder.name);
    scope2_body.open(&scope1_binder.name);
    scope2_binder.name = scope1_binder.name.clone();

    (scope1_binder, scope1_body, scope2_binder, scope2_body)
}
