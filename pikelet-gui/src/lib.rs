use iced::{Column, Container, Element, Sandbox, Text};

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
                .push(Text::new(""))
                .push(
                    globals
                        .entries()
                        .fold(Column::new(), |column, (name, (r#type, term))| match term {
                            None => column.push(Text::new(format!(
                                "{} : {type:?}", // TODO: pretty print?
                                name,
                                r#type = r#type,
                            ))),
                            Some(term) => column.push(Text::new(format!(
                                "{} : {type:?} = {term:?}", // TODO: pretty print?
                                name,
                                r#type = r#type,
                                term = term,
                            ))),
                        }),
                ),
        )
        .into()
    }
}
