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
    // TODO: multi-threading
    // FIXME: Just sketching this out - this currently doesn't work! :/

    {
        let stdin = std::io::stdin();
        let init_content = rpc::recv_content(&mut stdin.lock())?;
        match serde_json::from_str::<rpc::JsonRpc<rpc::LspCommand>>(&init_content) {
            Ok(rpc::JsonRpc {
                result: rpc::LspCommand::Initialize { .. },
                ..
            }) => {},
            Ok(_) => unimplemented!(),
            Err(_) => unimplemented!(),
        }
    }

    {
        let stdout = std::io::stdout();
        let capabilities = server_capabilities();
        let init_resp = rpc::JsonRpc::new(0, lsp_ty::InitializeResult { capabilities });
        rpc::send_content(&mut stdout.lock(), serde_json::to_string(&init_resp)?)?;
    }

    // loop {
    //     match rpc::recv_content(&mut stdin) {
    //         Ok(_) => unimplemented!(),
    //         Err(error) => eprintln!("error: {}", error),
    //     }
    // }

    Ok(())
}
