#!/usr/bin/env fish

cat compile_flags.txt | xargs gcc main.c -o task
