#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Symbol {
    pub r#type: Type,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
    Variable,
}
