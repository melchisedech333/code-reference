#!/bin/bash

clear
rm -rf lib_area1.rlib lib_lib1.rlib main

# rustc --crate-type=lib --crate-name=_area1 area1.rs
# rustc --crate-type=lib --crate-name=_lib1  lib1.rs 

rustc main.rs

ls


