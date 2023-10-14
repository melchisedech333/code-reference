
pub trait Summary {
    
    // Define método padrão.
    fn summarize(&self) -> String {
        format!("(Read more from {}...)", self.summarize_author()) // Faz chamada a um método
                                                                   // que não é definido como padrão.
    }

    // Define método padrão: que será sobrescrito por uma implementação futura.
    fn default_method(&self) {
        println!("Default trait function!");
    }

    // Define um método, mas não como padrão.
    // Este pode ser re-declarado pelas implementações da Trait.    
    fn summarize_author(&self) -> String;
}

pub struct NewsArticle {
    pub headline: String,
    pub location: String,
    pub author: String,
    pub content: String,
}

// Define implementação, mas não necessita declarar o método 'summarize'.
// Ainda assim, quando ele for chamado, o método existirá, pois foi definido
// como padrão da definição da própria Trait Summary.
impl Summary for NewsArticle {
    fn summarize_author(&self) -> String {
        format!("@{}", self.author)
    }

    // Sobrescreve método.
    fn default_method(&self) {
        
        println!("Default trait function - NewsArticle!");
    }
}

pub struct Tweet {
    pub username: String,
    pub content: String,
    pub reply: bool,
    pub retweet: bool,
}

impl Summary for Tweet {
    
    // O mesmo para esta implementação, onde apenas o author é modificado/diferente.
    fn summarize_author(&self) -> String {
        format!("@{}", self.username)
    }
}

fn main() {
    let tweet = Tweet {
        username: String::from("tweet_user"),
        content: String::from(
            "of course, as you probably already know, people",
        ),
        reply: false,
        retweet: false,
    };

    let news_article = NewsArticle {
        headline: String::from("headline..."),
        location: String::from("location..."),
        author: String::from("article_author"),
        content: String::from("content..."),
    };

    println!("tweet.......: {}", tweet.summarize());
    println!("news article: {}", news_article.summarize());

    // Chamando a implementação padrão.
    tweet.default_method();
    news_article.default_method();
}


