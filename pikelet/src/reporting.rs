use crate::lang::core;
use crate::pass::surface_to_core;

use codespan_reporting::diagnostic::Diagnostic;
use pretty::DocAllocator;

#[derive(Clone, Debug)]
pub enum Message {
    CoreTyping(core::typing::Message),
    SurfaceToCore(surface_to_core::Message),
}

impl From<core::typing::Message> for Message {
    fn from(message: core::typing::Message) -> Self {
        Message::CoreTyping(message)
    }
}

impl From<surface_to_core::Message> for Message {
    fn from(message: surface_to_core::Message) -> Self {
        Message::SurfaceToCore(message)
    }
}

impl Message {
    pub fn to_diagnostic<'a, D>(&'a self, pretty_alloc: &'a D) -> Diagnostic<()>
    where
        D: DocAllocator<'a>,
        D::Doc: Clone,
    {
        match self {
            Message::CoreTyping(message) => message.to_diagnostic(pretty_alloc),
            Message::SurfaceToCore(message) => message.to_diagnostic(pretty_alloc),
        }
    }
}
