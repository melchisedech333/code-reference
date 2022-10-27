
// IHS <3

pub fn public_function() {
    println!("called public_function()");
}

pub fn indirect_access() {
    println!("called indirect_access()");
    private_function();
}

fn private_function() {
    println!("called private_function()");
}


