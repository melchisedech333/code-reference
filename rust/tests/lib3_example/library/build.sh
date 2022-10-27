#!/bin/bash

clear
rm -rf *.rlib

rustc --crate-type=lib --crate-name=_library library.rs

ls


