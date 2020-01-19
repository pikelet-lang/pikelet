use iced::{Column, Container, Element, Row, Sandbox, Settings, Text};

pub fn run() {
    Workspace::run(Settings::default())
}

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
        "Pikelet".to_owned()
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
    use pikelet::core::{Constant, Term, UniverseLevel, UniverseOffset};

    match term {
        Term::Universe(UniverseLevel(level)) => Row::new()
            .push(Text::new(format!("Univ^{}", level))) // TODO: superscript?
            .into(),
        Term::Global(name) => Text::new(name).into(),
        Term::Local(_) => Text::new("todo").into(),
        Term::Constant(Constant::U8(data)) => Text::new(data.to_string()).into(),
        Term::Constant(Constant::U16(data)) => Text::new(data.to_string()).into(),
        Term::Constant(Constant::U32(data)) => Text::new(data.to_string()).into(),
        Term::Constant(Constant::U64(data)) => Text::new(data.to_string()).into(),
        Term::Constant(Constant::S8(data)) => Text::new(data.to_string()).into(),
        Term::Constant(Constant::S16(data)) => Text::new(data.to_string()).into(),
        Term::Constant(Constant::S32(data)) => Text::new(data.to_string()).into(),
        Term::Constant(Constant::S64(data)) => Text::new(data.to_string()).into(),
        Term::Constant(Constant::F32(data)) => Text::new(data.to_string()).into(),
        Term::Constant(Constant::F64(data)) => Text::new(data.to_string()).into(),
        Term::Constant(Constant::Char(data)) => Text::new(format!("{:?}", data)).into(),
        Term::Constant(Constant::String(data)) => Text::new(format!("{:?}", data)).into(),
        Term::Sequence(_) => Text::new("todo").into(),
        Term::Ann(term, r#type) => Row::new()
            .push(view_term(term))
            .push(Text::new(" : "))
            .push(view_term(r#type))
            .into(),
        Term::RecordTerm(_) => Text::new("todo").into(),
        Term::RecordType(_) => Text::new("todo").into(),
        Term::RecordElim(_, _) => Text::new("todo").into(),
        Term::FunctionType(_, _) => Text::new("todo").into(),
        Term::FunctionTerm(_, _) => Text::new("todo").into(),
        Term::FunctionElim(_, _) => Text::new("todo").into(),
        Term::Lift(term, UniverseOffset(offset)) => Row::new()
            .push(view_term(term))
            .push(Text::new(format!("^{}", offset)))
            .into(),
        Term::Error => Text::new("ERROR!").into(),
    }
}
