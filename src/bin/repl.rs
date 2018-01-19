extern crate lambdapi;
extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use lambdapi::parse::ParseError;
use lambdapi::core::{EvalError, FromParseError, RcITerm};
use lambdapi::check::TypeError;

const PROMPT: &str = "λΠ> ";
const REPL_HISTORY_FILE: &str = "repl-history";

fn main() {
    let mut rl = Editor::<()>::new();

    if let Err(_) = rl.load_history(REPL_HISTORY_FILE) {}

    loop {
        match rl.readline(PROMPT) {
            Ok(line) => {
                rl.add_history_entry(&line);

                match run_repl(&line) {
                    Ok(()) => {}
                    Err(ReplError::Parse(err)) => println!("parse error: {}", err.0),
                    Err(ReplError::FromParse(FromParseError)) => {
                        println!("not enough type annotations")
                    }
                    Err(ReplError::Eval(err)) => println!("eval error: {:?}", err),
                    Err(ReplError::Type(err)) => println!("type error: {:?}", err),
                }
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

fn run_repl(line: &str) -> Result<(), ReplError> {
    use lambdapi::check::Context;
    use lambdapi::pretty::{self, ToDoc};
    use lambdapi::parse::ReplCommand;

    match line.parse()? {
        ReplCommand::NoOp => {}
        ReplCommand::Help => {
            println!("<expr>       evaluate a term");
            println!(":h           print command help");
            println!(":t <expr>    infer the type of an expression");
        }
        ReplCommand::TypeOf(parse_term) => {
            let term = RcITerm::from_parse(&parse_term)?;
            let inferred = Context::default().infer(&term)?;

            println!("{}", inferred.to_doc(pretty::Context::default()).pretty(80));
        }
        ReplCommand::Eval(parse_term) => {
            let term = RcITerm::from_parse(&parse_term)?;
            let inferred = Context::default().infer(&term)?;
            let evaluated = term.eval()?;
            let doc = pretty::pretty_ann(pretty::Context::default(), &evaluated, &inferred);

            println!("{}", doc.pretty(80));
        }
    }

    Ok(())
}

enum ReplError {
    Parse(ParseError),
    FromParse(FromParseError),
    Eval(EvalError),
    Type(TypeError),
}

impl From<ParseError> for ReplError {
    fn from(src: ParseError) -> ReplError {
        ReplError::Parse(src)
    }
}

impl From<FromParseError> for ReplError {
    fn from(src: FromParseError) -> ReplError {
        ReplError::FromParse(src)
    }
}

impl From<EvalError> for ReplError {
    fn from(src: EvalError) -> ReplError {
        ReplError::Eval(src)
    }
}

impl From<TypeError> for ReplError {
    fn from(src: TypeError) -> ReplError {
        ReplError::Type(src)
    }
}
