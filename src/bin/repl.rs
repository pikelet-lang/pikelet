extern crate lambdapi;
extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use lambdapi::parse::ParseError;
use lambdapi::core::RcTerm;
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
                    Ok(()) => {},
                    Err(ReplError::Parse(err)) => println!("parse error: {}", err.0),
                    Err(ReplError::Type(err)) => println!("type error: {:?}", err),
                    Err(ReplError::Quit) => {
                        println!("Bye bye");
                        break;
                    },
                }
            },
            Err(err) => match err {
                ReadlineError::Interrupted => println!("Interrupt"),
                ReadlineError::Eof => {
                    println!("Bye bye");
                    break;
                },
                err => {
                    println!("Error: {:?}", err);
                    break;
                },
            },
        }
    }

    rl.save_history(REPL_HISTORY_FILE).unwrap();
}

fn run_repl(line: &str) -> Result<(), ReplError> {
    use lambdapi::check::Context;
    use lambdapi::pretty::{self, ToDoc};
    use lambdapi::parse::ReplCommand;

    match line.parse()? {
        ReplCommand::Help => {
            println!("");
            println!("Command       Arguments   Purpose");
            println!("");
            println!("<expr>                    evaluate a term");
            println!(":? :h :help               display this help text");
            println!(":q :quit                  quit the repl");
            println!(":t :type      <expr>      infer the type of an expression");
            println!("");
        },

        ReplCommand::Eval(parse_term) => {
            let term = RcTerm::from_parse(&parse_term);
            let context = Context::new();
            let inferred = context.infer(&term)?;
            let evaluated = context.eval(&term);
            let doc = pretty::pretty_ann(pretty::Context::default(), &evaluated, &inferred);

            println!("{}", doc.pretty(80));
        },
        ReplCommand::TypeOf(parse_term) => {
            let term = RcTerm::from_parse(&parse_term);
            let inferred = Context::new().infer(&term)?;
            let doc = inferred.to_doc(pretty::Context::default());

            println!("{}", doc.pretty(80));
        },

        ReplCommand::NoOp => {},
        ReplCommand::Quit => return Err(ReplError::Quit),
    }

    Ok(())
}

enum ReplError {
    Parse(ParseError),
    Type(TypeError),
    Quit,
}

impl From<ParseError> for ReplError {
    fn from(src: ParseError) -> ReplError {
        ReplError::Parse(src)
    }
}

impl From<TypeError> for ReplError {
    fn from(src: TypeError) -> ReplError {
        ReplError::Type(src)
    }
}
