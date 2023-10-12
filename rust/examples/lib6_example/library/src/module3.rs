
// IHS <3

// Public interface.
pub fn module3_pub_function() {
    println!("called module3::module3_pub_function()");
}

pub fn module3_ind_function() {
    println!("called module3::module3_ind_function()");

    module3_priv_function();
}

// Internal implementation.
fn module3_priv_function() {
    println!("called module3::module3_priv_function()");
}


