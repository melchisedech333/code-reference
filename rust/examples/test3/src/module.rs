// Trait.
pub trait Summary {
    fn summarize(&self) -> String {
        format!("(Read more from {}...)", self.summarize_author())
    }

    fn summarize_author(&self) -> String;
    fn add(&self, a: i32, b: i32) -> i32;
}

// Tweet implementation.
pub struct Tweet {
    pub username: String,
}

impl Summary for Tweet {
    fn summarize_author(&self) -> String {
        format!("@{}", self.username)
    }

    fn add(&self, a: i32, b: i32) -> i32 {
        a + b
    }
}
