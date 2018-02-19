use source::CodeMap;

use Diagnostic;

pub fn emit(codemap: &CodeMap, diagnostic: &Diagnostic) {
    println!("{}: {}", diagnostic.severity, diagnostic.message);
    for span_label in &diagnostic.spans {
        let file = codemap.find_file(span_label.span.lo()).expect("find_file");
        let (line, col) = file.location(span_label.span.lo()).expect("location");

        print!("- {}:{}:{}", file.name(), line.number(), col.number());
        match span_label.label {
            None => println!(),
            Some(ref label) => println!(": {}", label),
        }
    }
}
