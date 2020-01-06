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
                            column.push({
                                let entry = Row::new()
                                    .push(Text::new(name))
                                    .push(Text::new(" : "))
                                    .push(view_term(r#type));

                                match term {
                                    None => entry.push(Text::new("")).push(Text::new("")),
                                    Some(term) => {
                                        entry.push(Text::new(" = ")).push(view_term(term))
                                    }
                                }
                            })
                        }),
                ),
        )
        .into()
    }
}

fn view_term<M: 'static>(term: &pikelet::core::Term) -> Element<M> {
    use pikelet::core::Term;

    match term {
        Term::Universe(level) => Row::new()
            .push(Text::new(format!("Type^{}", level.0))) // TODO: superscript?
            .into(),
        Term::Global(name) => Text::new(name).into(),
        Term::Constant(_) => Text::new("todo").into(),
        Term::Sequence(_) => Text::new("todo").into(),
        Term::Ann(_, _) => Text::new("todo").into(),
        Term::RecordTerm(_) => Text::new("todo").into(),
        Term::RecordType(_) => Text::new("todo").into(),
        Term::RecordElim(_, _) => Text::new("todo").into(),
        Term::ArrayType(_, _) => Text::new("todo").into(),
        Term::ListType(_) => Text::new("todo").into(),
        Term::Lift(term, shift) => Row::new()
            .push(view_term(term))
            .push(Text::new(format!("^{}", shift.0)))
            .into(),
        Term::Error => Text::new("ERROR!").into(),
    }
}
