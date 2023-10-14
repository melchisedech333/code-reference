mod module;
mod module_tests;

use module::{Summary, Tweet};

fn main() {
    let tweet = Tweet {
        username: String::from("tweet_user"),
    };
    println!("tweet: {}", tweet.summarize());
    println!("value: {}", tweet.add(10, 10));
}
