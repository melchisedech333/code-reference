#!/bin/bash

iverilog -o gate basic.v basic-test.v
vvp gate


