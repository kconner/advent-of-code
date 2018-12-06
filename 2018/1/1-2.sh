#!/bin/sh
yes "$(<input.txt)" | awk 'BEGIN { FS = null } { sum += $0 ; if( sum in seen ) { exit } else { seen[sum] = 1 } } END { print sum }' 
