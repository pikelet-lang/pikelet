use source::CodeMap;

use Diagnostic;

pub fn emit(codemap: &CodeMap, diagnostic: &Diagnostic) {
    println!("{}: {}", diagnostic.severity, diagnostic.message);
    for span_label in &diagnostic.spans {
        match codemap.find_file(span_label.span.lo()) {
            None => if let Some(ref label) = span_label.label {
                println!("- {}", label)
            },
            Some(file) => {
                let (line, col) = file.location(span_label.span.lo()).expect("location");

                print!("- {}:{}:{}", file.name(), line.number(), col.number());
                match span_label.label {
                    None => println!(),
                    Some(ref label) => println!(": {}", label),
                }
            },
        }
    }
}
