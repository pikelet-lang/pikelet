module prelude;


id : (a : Type) -> a -> a;
id a x = x;

const : (a : Type) -> (b : Type) -> a -> b -> a;
const a b x y = x;

compose : (a : Type) -> (b : Type) -> (c : Type) -> (b -> c) -> (a -> b) -> (a -> c);
compose a b c f g x = f (g x);

flip : (a : Type) -> (b : Type) -> (c : Type) -> (a -> b -> c) -> (b -> a -> c);
flip a b c f x y = f y x;


void : Type;
void = (a : Type) -> a;

unit : Type;
unit = (a : Type) -> a -> a;

unit-intro : unit;
unit-intro = id;

unit-elim : (a : Type) -> unit -> a -> a;
unit-elim a f x = f a x;


and : Type -> Type -> Type;
and p q = (c : Type) -> (p -> q -> c) -> c;

and-intro : (p : Type) -> (q : Type) -> p -> q -> and p q;
and-intro p q x y c f = f x y;

and-elim-left : (p : Type) -> (q : Type) -> and p q -> p;
and-elim-left p q (pq : and p q) = pq p const;

and-elim-right : (p : Type) -> (q : Type) -> and p q -> q;
and-elim-right p q (pq : and p q) = pq p (flip const);


or : Type -> Type -> Type;
or p q = (c : Type) -> (p -> c) -> (q -> c) -> c;

or-intro-left : (p : Type) -> (q : Type) -> p -> or p q;
or-intro-left p q (x : p) c (on-p : p -> c) (on-q : q -> c) = on-p x;

or-intro-right : (p : Type) -> (q : Type) -> q -> or p q;
or-intro-right p q (y : q) c (on-p : p -> c) (on-q : q -> c) = on-q y;
