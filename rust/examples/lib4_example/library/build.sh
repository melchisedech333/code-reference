#!/bin/bash

clear
rm -rf *.rlib

rustc --crate-type=lib --crate-name=_library --edition=2021 library.rs

ls


