extern crate lambdapi;
extern crate pretty;
extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

const PROMPT: &str = "λΠ> ";
const REPL_HISTORY_FILE: &str = "repl-history";

fn main() {
    let mut rl = Editor::<()>::new();

    if let Err(_) = rl.load_history(REPL_HISTORY_FILE) {}

    loop {
        match rl.readline(PROMPT) {
            Ok(line) => {
                use pretty::Doc;
                use lambdapi::check::Context;
                use lambdapi::pretty::{Prec, ToDoc, INDENT_WIDTH};
                use lambdapi::parse::Term;

                rl.add_history_entry(&line);

                // TODO: Add the following command syntax:
                //
                // cmd ::= <expr>       evaluate a term
                //       | :h           print command help
                //       | :t <expr>    return the type of an expression

                // TODO: Clean up this stuff!

                if line.is_empty() {
                    continue;
                }

                let term = match line.parse() {
                    Ok(term) => term,
                    Err(err) => {
                        println!("parse error: {:?}", err);
                        continue;
                    }
                };

                let core = match Term::to_core(&term) {
                    Ok(core) => core,
                    Err(err) => {
                        println!("parse error: {:?}", err);
                        continue;
                    }
                };

                let ty = match Context::default().infer(&core) {
                    Ok(ty) => ty,
                    Err(err) => {
                        println!("type error: {:?}", err);
                        continue;
                    }
                };

                let result = Doc::nil()
                    .append(core.to_doc(Prec::ANN))
                    .append(Doc::space())
                    .append(Doc::text(":"))
                    .group()
                    .append(Doc::space())
                    .append(ty.to_doc(Prec::ANN).nest(INDENT_WIDTH))
                    .group();

                println!("{}", result.pretty(80));
            }
            Err(err) => {
                match err {
                    ReadlineError::Interrupted => println!("CTRL-C"),
                    ReadlineError::Eof => println!("CTRL-D"),
                    err => println!("Error: {:?}", err),
                }
                break;
            }
        }
    }

    rl.save_history(REPL_HISTORY_FILE).unwrap();
}
