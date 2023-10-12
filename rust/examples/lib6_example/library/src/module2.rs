
// IHS <3

use crate::module3::*;

// Public interface.
pub fn module2_pub_function() {
    println!("called module2::module2_pub_function()");
}

pub fn module2_ind_function() {
    println!("called module2::module2_ind_function()");

    module2_priv_function();
}

// Internal implementation.
fn module2_priv_function() {
    println!("called module2::module2_priv_function()");

    module3_pub_function();
    module3_ind_function();
}


