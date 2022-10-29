#!/bin/bash

rm -rf *.beam

erl -compile app.erl
erl -noshell -s app app -s init stop


