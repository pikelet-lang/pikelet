use pretty::Doc;

use syntax::context::{Context, Entry};

use super::{parens, sexpr, StaticDoc, ToDoc};

impl ToDoc for Entry {
    fn to_doc(&self) -> StaticDoc {
        match *self {
            Entry::Claim(ref name, ref ty) => sexpr(
                "claim",
                Doc::text(format!("{:#}", name))
                    .append(Doc::space())
                    .append(ty.to_doc()),
            ),
            Entry::Definition(ref name, ref term) => sexpr(
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
