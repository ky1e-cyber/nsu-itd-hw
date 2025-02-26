#!/usr/bin/env fish

cat compile_flags.txt | xargs gcc $argv[1] -o task && ./task $argv[2..-1]
