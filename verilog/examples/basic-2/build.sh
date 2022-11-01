#!/bin/bash

iverilog -o basic basic.v basic-test.v
vvp basic
rm -rf basic


