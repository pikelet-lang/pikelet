extern crate pikelet_driver;
extern crate pikelet_library;

use pikelet_driver::termcolor::{ColorChoice, StandardStream};
use pikelet_driver::{Driver, FileName};

#[test]
fn with_prelude() {
    let _driver = Driver::with_prelude();
}

#[test]
fn prelude() {
    let mut driver = Driver::new();
    let writer = StandardStream::stdout(ColorChoice::Always);

    if let Err(diagnostics) = driver.register_file(
        "prim".to_owned(),
        FileName::virtual_("prim"),
        pikelet_library::PRIM.to_owned(),
    ) {
        driver.emit(writer.lock(), &diagnostics).unwrap();
        panic!("load error!")
    }

    if let Err(diagnostics) = driver.register_file(
        "prelude".to_owned(),
        FileName::virtual_("prelude"),
        pikelet_library::PRELUDE.to_owned(),
    ) {
        driver.emit(writer.lock(), &diagnostics).unwrap();
        panic!("load error!")
    }
}
