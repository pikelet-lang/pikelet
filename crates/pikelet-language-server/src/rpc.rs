//! Base RPC handling for the Language Server Protocol
//!
//! # Example message
//!
//! ```text
//! Content-Length: ...\r\n
//! \r\n
//! {
//!     "jsonrpc": "2.0",
//!     "id": 1,
//!     "method": "textDocument/didOpen",
//!     "params": {
//!         ...
//!     }
//! }
//! ```
//!
//! # References
//!
//! - [Language Server Protocol Specification: Base Protocol][base-protocol]
//! - [JSON-RPC 2.0 Specification][json-rpc]
//!
//! [base-protocol]: https://microsoft.github.io/language-server-protocol/specification#base-protocol
//! [json-rpc]: https://www.jsonrpc.org/specification

use lsp_ty;
use serde::Serialize;
use std::io::{self, BufRead, Write};

#[allow(dead_code)]
pub fn send<T: Serialize>(writer: &mut impl Write, content: String) -> Result<(), io::Error> {
    let content_length = content.len();
    let content_type = "application/vscode-jsonrpc; charset=utf-8";

    // Header part
    //
    // https://microsoft.github.io/language-server-protocol/specification#header-part

    write!(writer, "Content-Length: {}\r\n", content_length)?;
    write!(writer, "Content-Type: {}\r\n", content_type)?;
    write!(writer, "\r\n")?;

    // Content part
    //
    // https://microsoft.github.io/language-server-protocol/specification#content-part

    write!(writer, "{}", content)?;

    writer.flush()?;

    Ok(())
}

#[allow(dead_code)]
pub fn recv(reader: &mut impl BufRead) -> Result<String, io::Error> {
    // Header part
    //
    // https://microsoft.github.io/language-server-protocol/specification#header-part
    //
    // content-length   ::= "Content-Length: " length "\r\n"
    // content-type     ::= "Content-Type: " string "\r\n"
    // unknown          ::= string ": " string "\r\n"
    // header           ::= content-length / content-type / unknown
    // headers          ::= header headers / "\r\n"

    let content_len = None::<usize>; // TODO
    let charset = None::<&str>; // TODO

    let mut header_buffer = String::new();
    loop {
        reader.read_line(&mut header_buffer)?;
        match header_buffer.as_str() {
            "\r\n" => break,
            _ => {
                // TODO: parse content-length
                // TODO: parse content-type
                eprintln!("skipping")
            },
        }
    }

    // Content part
    //
    // https://microsoft.github.io/language-server-protocol/specification#content-part
    match content_len {
        Some(content_len) => {
            // Read into a pre-allocated buffer
            let mut buffer = Vec::with_capacity(content_len + 2); // why do we need to add 2?
            reader.read_exact(&mut buffer)?;

            match charset {
                // Map `utf8` to `utf-8` for backwards compatibility
                // If no charset is given default to `utf-8`
                Some("utf-8") | Some("utf8") | None => String::from_utf8(buffer)
                    .map_err(|error| io::Error::new(io::ErrorKind::InvalidInput, error)),
                // Should be fine to continue after this, because we've already
                // consumed the buffer
                Some(charset) => Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("Unknown charset: {}", charset),
                )),
            }
        },
        // FIXME: Can we recover from this? We'd need to try to skip to the
        // next thing that looks like a header :/
        None => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "missing content length",
        )),
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct JsonRpc<T> {
    pub jsonrpc: String,
    pub id: usize,
    pub result: T,
}

impl<T> JsonRpc<T> {
    #[allow(dead_code)]
    pub fn new(id: usize, result: T) -> JsonRpc<T> {
        JsonRpc {
            jsonrpc: "2.0".into(),
            id,
            result,
        }
    }
}

/// A Command that was sent from the client to the server
#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "method")]
pub enum LspCommand {
    #[serde(rename = "initialize")]
    Initialize {
        id: usize,
        params: lsp_ty::InitializeParams,
    },
    #[serde(rename = "initialized")]
    Initialized,
    #[serde(rename = "textDocument/didOpen")]
    DidOpen {
        params: lsp_ty::DidOpenTextDocumentParams,
    },
    #[serde(rename = "textDocument/didChange")]
    DidChange {
        params: lsp_ty::DidChangeTextDocumentParams,
    },
    #[serde(rename = "textDocument/hover")]
    Hover {
        id: usize,
        params: lsp_ty::TextDocumentPositionParams,
    },
    #[serde(rename = "textDocument/completion")]
    Completion {
        id: usize,
        params: lsp_ty::CompletionParams,
    },
    #[serde(rename = "$/cancelRequest")]
    CancelRequest { params: lsp_ty::CancelParams },
    #[serde(rename = "completionItem/resolve")]
    CompletionItemResolve {
        id: usize,
        params: lsp_ty::CompletionItem,
    },
}
