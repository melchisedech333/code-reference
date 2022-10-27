
// IHS <3

use super::area1::*;

pub fn public_function() {
    println!("called lib::public_function()");
}

pub fn indirect_access() {
    println!("called lib::indirect_access()");

    area1_indirect_access();
}


