
// Trait.
pub trait Summary {
    fn summarize(&self) -> String {
        format!("(Read more from {}...)", self.summarize_author())
    }

    fn default_method(&self) {
        println!("Default trait function!");
    }

    fn summarize_author(&self) -> String;
    
    // Default method.
    fn add(&self, a: i32, b: i32) -> i32 {
        a + b
    }
}

// Tweet implementation.
pub struct Tweet {
    pub username: String
}

impl Summary for Tweet {
    fn summarize_author(&self) -> String {
        format!("@{}", self.username)
    }
}

// Tests Tweet implementation.
#[cfg(test)]
mod tests {
    use crate::module::{ Summary, Tweet };

    #[test]
    fn test_check_attributes() {
        let tweet = Tweet { username: String::from("tweet_user") };
        assert_eq!(tweet.username, "tweet_user");
    }

    #[test]
    fn test_add() {
        let tweet = Tweet { username: String::from("tweet_user") };
        assert_eq!(tweet.add(3, 2), 5);
    }
}


