use iced::{Container, Element, Sandbox, Text};

#[derive(Debug, Clone)]
pub enum Message {}

pub struct Workspace {}

impl Sandbox for Workspace {
    type Message = Message;

    fn new() -> Workspace {
        Workspace {}
    }

    fn title(&self) -> String {
        format!("Pikelet")
    }

    fn update(&mut self, message: Message) {
        match message {}
    }

    fn view(&mut self) -> Element<Message> {
        let Workspace {} = self;

        Container::new(Text::new("Hi this is Pikelet!")).into()
    }
}
