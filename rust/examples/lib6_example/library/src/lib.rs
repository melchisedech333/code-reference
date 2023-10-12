
// IHS <3

// Public Crate Interface.

mod module1;
mod module2;
mod module3;

use crate::module1::*;

// Public interface.
pub fn library_pub_function() {
    println!("called library::library_pub_function()");
}

pub fn library_ind_function() {
    println!("called library::library_ind_function()");

    library_priv_function();
}

// Internal implementation.
fn library_priv_function() {
    println!("called library::library_priv_function()");

    // Call module1 functions.
    module1_pub_function();
    module1_ind_function();
}


