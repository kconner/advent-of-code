#!/bin/bash

echo '2
3' | xargs -I COUNT bash -c "
    cat input.txt | xargs -I LINE bash -c \"
        echo LINE | sed -E 's/(.)/\\\\
\1/g' | sort | uniq -c | cut -c 1-4 | sort | uniq | grep -w COUNT
    \" | wc -l
" | paste -s - | xargs -I FACTORS dc -e 'FACTORS * p'

