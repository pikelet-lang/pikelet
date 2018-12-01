pub mod concrete;
pub mod raw;

const PRETTY_INDENT_WIDTH: usize = 4;

/// An effectively 'infinite' line length for when we don't have an explicit
/// width provided for pretty printing.
///
/// `pretty.rs` seems to bug-out and break on every line when using
/// `usize::MAX`, so we'll just use a really big number instead...
pub const PRETTY_FALLBACK_WIDTH: usize = 1_000_000;
