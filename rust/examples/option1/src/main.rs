
// IHS s2

fn main() {
    let name = String::from("naufil");
    
    println!(
        "Character at index 6: {}",

        // Uso direto na match.
        match name.chars().nth(6) {
            Some(c) => c.to_string(),
            None => "No character at index 6!".to_string(),
        }
    )
}


