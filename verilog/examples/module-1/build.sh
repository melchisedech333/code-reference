#!/bin/bash

rm -rf arbiter
iverilog -o arbiter arbiter.v testbench.v
vvp arbiter
rm -rf arbiter


