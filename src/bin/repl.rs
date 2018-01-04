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
                use lambdapi::check::Context;
                use lambdapi::pretty;
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

                let inferred = match Context::default().infer(&core) {
                    Ok(ty) => ty,
                    Err(err) => {
                        println!("type error: {:?}", err);
                        continue;
                    }
                };

                let evaluated = core.eval().unwrap();
                let doc = pretty::pretty_ann(pretty::Context::default(), &*evaluated, &*inferred);

                println!("{}", doc.pretty(80));
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
