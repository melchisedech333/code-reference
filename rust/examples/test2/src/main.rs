
mod module;

use module::{ Summary, Tweet };

fn main() {
    let tweet = Tweet { username: String::from("tweet_user") };
    println!("tweet: {}", tweet.summarize());
    tweet.default_method();
    println!("value: {}", tweet.add(10, 10));
}


