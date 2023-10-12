#!/bin/bash

clear
rm -rf main

rustc main.rs --extern library=../library/lib_library.rlib

ls


