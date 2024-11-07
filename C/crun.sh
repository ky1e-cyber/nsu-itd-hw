#!/bin/bash

(cat compile_flags.txt | xargs gcc $1 -o task) && ./task  
