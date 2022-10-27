
// IHS <3

use crate::module2::*;

// Public interface.
pub fn module1_pub_function() {
    println!("called module1::module1_pub_function()");
}

pub fn module1_ind_function() {
    println!("called module1::module1_ind_function()");

    module1_priv_function();
}

// Internal implementation.
fn module1_priv_function() {
    println!("called module1::module1_priv_function()");

    module2_pub_function();
    module2_ind_function();
}


