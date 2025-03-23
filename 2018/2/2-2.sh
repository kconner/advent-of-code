#!/bin/bash

diff -y <(echo LINE1 | sed -E 's/(.)/\1 /g' | xargs -n 1) <(echo LINE2 | sed -E 's/(.)/\1 /g' | xargs -n 1) | grep '|' | wc -l

cat input.txt | xargs -I LINE1 bash -c "
    cat input.txt | grep -n -m 1 LINE1 | cut -f 1 -d ':' | xargs -I INDEX head -INDEX input.txt | grep -v LINE1 | xargs -I LINE2 bash -c \"
        diff -y <(echo LINE1 | sed -E 's/(.)/\1 /g' | xargs -n 1) <(echo LINE2 | sed -E 's/(.)/\1 /g' | xargs -n 1) | grep '|' | wc -l    
    \"
"

