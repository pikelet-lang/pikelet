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
use std::io::{self, BufRead, Write};

/// Sends an RPC call containing the given content
#[allow(dead_code)]
pub fn send_content(writer: &mut impl Write, content: String) -> Result<(), io::Error> {
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

/// Receives an RPC call from the given reader, returning the content as a string
#[allow(dead_code)]
pub fn recv_content(reader: &mut impl BufRead) -> Result<String, io::Error> {
    // Header part
    //
    // https://microsoft.github.io/language-server-protocol/specification#header-part
    //
    // content-length   ::= "Content-Length: " length "\r\n"
    // content-type     ::= "Content-Type: " string "\r\n"
    // unknown          ::= string ": " string "\r\n"
    // header           ::= content-length / content-type / unknown
    // headers          ::= header headers / "\r\n"

    let mut content_len = None::<usize>;
    let charset = None::<&str>; // TODO

    // Loop through headers, collecting the relevant information
    let mut header_buffer = String::new();
    loop {
        reader.read_line(&mut header_buffer)?;
        {
            let mut splits = header_buffer.splitn(2, ": ");
            match (splits.next(), splits.next()) {
                // Content-Length header
                (Some("Content-Length"), Some(value)) => {
                    if content_len.is_none() {
                        content_len = Some(value.trim_end().parse().map_err(|err| {
                            io::Error::new(
                                io::ErrorKind::InvalidData,
                                format!("`Content-Length` was not a valid number: {:?}", err),
                            )
                        })?);
                    }
                },
                // Content-Type header
                (Some("Content-Type"), Some(_)) => {}, // TODO: parse content type?
                // Other headers, skipped to ensure forwards compatibility
                (Some(name), Some(_)) => eprintln!("Skipping unknown header: {:?}", name),
                // End of the headers
                (Some("\r\n"), None) => break,
                (Some(header), None) => eprintln!("Skipping malformed header: {:?}", header),
                (None, _) => eprintln!("Malformed header, skipping"),
            }
        }
        header_buffer.clear();
    }

    // Content part
    //
    // https://microsoft.github.io/language-server-protocol/specification#content-part
    match content_len {
        Some(content_len) => {
            // Read the content into a pre-allocated buffer
            // let mut buffer = vec![0; content_len + 2]; // why do we need to add 2?
            let mut buffer = vec![0; content_len];
            reader.read_exact(&mut buffer)?;

            match charset {
                // Map `utf8` to `utf-8` for backwards compatibility
                // If no charset is given, we'll default to `utf-8`
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
            "Missing content length",
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

#[cfg(test)]
mod tests {
    use super::*;

    mod recv_content {
        use super::*;

        #[test]
        fn valid_empty_no_charset() {
            let message = "Content-Length: 0\r\n\r\n";
            let mut cursor = io::Cursor::new(message);
            assert_eq!(recv_content(&mut cursor).unwrap(), "");
        }

        #[test]
        fn valid_no_charset() {
            let message = "Content-Length: 13\r\n\r\nhello, world!";
            let mut cursor = io::Cursor::new(message);
            assert_eq!(recv_content(&mut cursor).unwrap(), "hello, world!");
        }

        #[test]
        fn valid_explicit_charset_utf_8() {
            let message =
                "Content-Length: 13\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\nhello, world!";
            let mut cursor = io::Cursor::new(message);
            assert_eq!(recv_content(&mut cursor).unwrap(), "hello, world!");
        }

        #[test]
        fn valid_explicit_charset_utf8() {
            let message =
                "Content-Length: 13\r\nContent-Type: application/vscode-jsonrpc; charset=utf8\r\n\r\nhello, world!";
            let mut cursor = io::Cursor::new(message);
            assert_eq!(recv_content(&mut cursor).unwrap(), "hello, world!");
        }

        #[test]
        fn valid_unknown_header() {
            let message = "Content-Length: 13\r\nX-Foo: silly\r\n\r\nhello, world!";
            let mut cursor = io::Cursor::new(message);
            assert_eq!(recv_content(&mut cursor).unwrap(), "hello, world!");
        }

        // TODO: test more combinations

        // #[test]
        // fn combinations() {
        //     let things = vec![
        //         ("Content-Length: 0\r\n\r\n", Ok("")),
        //         ("Content-Length: 13\r\n\r\nhello, world!", Ok("hello, world!")),
        //         ("Content-Length: 13\r\nX-Foo: silly\r\n\r\nhello, world!", Ok("hello, world!")),
        //         ("Content-Length: 13\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\nhello, world!", Ok("hello, world!")),
        //         ("Content-Length: 13\r\nContent-Type: application/vscode-jsonrpc; charset=utf8\r\n\r\nhello, world!", Ok("hello, world!")),
        //         ("Content-Type: application/vscode-jsonrpc; charset=utf-8\r\nContent-Length: 13\r\n\r\nhello, world!", Ok("hello, world!")),
        //         ("Content-Type: application/vscode-jsonrpc; charset=utf8\r\nContent-Length: 13\r\n\r\nhello, world!", Ok("hello, world!")),
        //         ("\r\nhello, world!", Err(_)),
        //         ("Content-Length: 13.0\r\n\r\nhello, world!", Err(_)),
        //         ("Content-Type: application/vscode-jsonrpc; charset=utf8\r\n\r\nhello, world!", Err(_)),
        //     ];
        // }
    }

    // #[test]
    // fn send_content_recv_content_roundtrip() {
    //     // TODO: finish
    //     // TODO: quickcheck?
    //     let content = "hello, world!";
    //     let result = unimplemented!();
    //     assert_eq!(content, result);
    // }
}
