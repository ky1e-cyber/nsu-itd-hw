#!/bin/bash

(cat compile_flags.txt | xargs clang $1 -o task) && ./task  
