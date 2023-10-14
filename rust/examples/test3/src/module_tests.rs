// Tests Tweet implementation.
#[cfg(test)]
mod tests {
    use crate::module::{Summary, Tweet};

    #[test]
    fn test_check_attributes() {
        let tweet = Tweet {
            username: String::from("tweet_user"),
        };
        assert_eq!(tweet.username, "tweet_user");
    }

    #[test]
    fn test_add() {
        let tweet = Tweet {
            username: String::from("tweet_user"),
        };
        assert_eq!(tweet.add(3, 2), 5);
    }
}
