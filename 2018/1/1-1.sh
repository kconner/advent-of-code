#!/bin/sh
awk 'BEGIN { FS = null } { sum += $0 } END { print sum }' input.txt
