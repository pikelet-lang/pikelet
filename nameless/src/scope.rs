use {AlphaEq, Binder, Bound, Debruijn};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope<B, T> {
    pub unsafe_binder: B,
    pub unsafe_body: T,
}

impl<B, T> Scope<B, T>
where
    B: Binder,
    T: Bound<FreeName = B::FreeName, BoundName = B::BoundName>,
{
    pub fn bind(binder: B, mut body: T) -> Scope<B, T> {
        body.close(&binder);

        Scope {
            unsafe_binder: binder,
            unsafe_body: body,
        }
    }

    pub fn unbind(self) -> (B, T) {
        let mut binder = self.unsafe_binder;
        let mut body = self.unsafe_body;

        binder.freshen();
        body.open(&binder);

        (binder, body)
    }
}

impl<B: AlphaEq, T: AlphaEq> AlphaEq for Scope<B, T> {
    fn alpha_eq(&self, other: &Scope<B, T>) -> bool {
        B::alpha_eq(&self.unsafe_binder, &other.unsafe_binder)
            && T::alpha_eq(&self.unsafe_body, &other.unsafe_body)
    }
}

impl<B, T> Bound for Scope<B, T>
where
    B: Binder,
    T: Bound<FreeName = B::FreeName, BoundName = B::BoundName>,
{
    type FreeName = B::FreeName;
    type BoundName = B::BoundName;

    fn close_at<B1>(&mut self, index: Debruijn, binder: &B1)
    where
        B1: Binder<FreeName = B::FreeName, BoundName = B::BoundName>,
    {
        self.unsafe_binder.close_at(index, binder);
        self.unsafe_body.close_at(index.succ(), binder);
    }

    fn open_at<B1>(&mut self, index: Debruijn, binder: &B1)
    where
        B1: Binder<FreeName = B::FreeName, BoundName = B::BoundName>,
    {
        self.unsafe_binder.open_at(index, binder);
        self.unsafe_body.open_at(index.succ(), binder);
    }
}

pub fn unbind2<B1, T1, B2, T2>(scope1: Scope<B1, T1>, scope2: Scope<B2, T2>) -> (B1, T1, B2, T2)
where
    B1: Binder,
    T1: Bound<FreeName = B1::FreeName, BoundName = B1::BoundName>,
    B2: Binder<FreeName = B1::FreeName, BoundName = B1::BoundName, NamePerm = B1::NamePerm>,
    T2: Bound<FreeName = B1::FreeName, BoundName = B1::BoundName>,
{
    let mut scope1_binder = scope1.unsafe_binder;
    let mut scope1_body = scope1.unsafe_body;
    let mut scope2_binder = scope2.unsafe_binder;
    let mut scope2_body = scope2.unsafe_body;

    {
        let names = scope1_binder.freshen();
        scope2_binder.rename(&names);
        scope1_body.open(&scope1_binder);
        scope2_body.open(&scope2_binder);
    }

    (scope1_binder, scope1_body, scope2_binder, scope2_body)
}
