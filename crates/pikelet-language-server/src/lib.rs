//! A language server for Pikelet

extern crate failure;
extern crate languageserver_types as lsp_ty;
extern crate pikelet_driver;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
#[macro_use]
extern crate structopt;

use failure::Error;

mod rpc;

#[derive(Debug, StructOpt)]
pub struct Opts {
    // TODO
}

fn server_capabilities() -> lsp_ty::ServerCapabilities {
    lsp_ty::ServerCapabilities {
        text_document_sync: None,
        hover_provider: None,
        completion_provider: None,
        signature_help_provider: None,
        definition_provider: None,
        type_definition_provider: None,
        implementation_provider: None,
        references_provider: None,
        document_highlight_provider: None,
        document_symbol_provider: None,
        workspace_symbol_provider: None,
        code_action_provider: None,
        code_lens_provider: None,
        document_formatting_provider: None,
        document_range_formatting_provider: None,
        document_on_type_formatting_provider: None,
        rename_provider: None,
        color_provider: None,
        folding_range_provider: None,
        execute_command_provider: None,
        workspace: None,
    }
}

/// Run `language-server` with the given options
pub fn run(_opts: Opts) -> Result<(), Error> {
    // TODO: recv initialize

    let _init_result = lsp_ty::InitializeResult {
        capabilities: server_capabilities(),
    };

    // TODO: send init_result
    // loop {
    //     match rpc::recv::<LspCommand>() {
    //         Ok(_) => unimplemented!(),
    //         Err(error) => eprintln!("error: {}", error),
    //     }
    // }

    Ok(())
}
