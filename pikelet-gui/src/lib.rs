use iced::{Column, Container, Element, Row, Sandbox, Text};

#[derive(Debug, Clone)]
pub enum Message {}

pub struct Workspace {
    globals: pikelet::core::Globals,
}

impl Sandbox for Workspace {
    type Message = Message;

    fn new() -> Workspace {
        Workspace {
            globals: pikelet::core::Globals::default(),
        }
    }

    fn title(&self) -> String {
        format!("Pikelet")
    }

    fn update(&mut self, message: Message) {
        match message {}
    }

    fn view(&mut self) -> Element<Message> {
        let Workspace { globals } = self;

        Container::new(
            Column::new()
                .push(Text::new("Hi this is Pikelet!"))
                .push(Text::new("Globals:"))
                .push(
                    globals
                        .entries()
                        .fold(Column::new(), |column, (name, (r#type, term))| {
                            column.push(
                                Row::new()
                                    .push(Text::new(name))
                                    .push(Text::new(format!(" : {:?}", r#type))) // TODO: pretty print?
                                    .push(match term {
                                        None => Text::new(""),
                                        Some(term) => Text::new(format!(" = {:?}", term)), // TODO: pretty print?
                                    }),
                            )
                        }),
                ),
        )
        .into()
    }
}
