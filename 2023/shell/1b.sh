#!/bin/zsh -euo pipefail

cat 1.txt | rev | sed -nE 's/^.*((eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|[1-9])).*$/\1/p' | rev > 1.first.txt
cat 1.txt | sed -nE 's/^.*((one|two|three|four|five|six|seven|eight|nine|[1-9])).*$/\1/p' > 1.second.txt
paste -d',' 1.first.txt 1.second.txt | sed 's/,/*10+/g' > 1.calibration-values.txt 

cat <(echo "one=1\ntwo=2\nthree=3\nfour=4\nfive=5\nsix=6\nseven=7\neight=8\nnine=9") \
  <(paste -s -d'+' 1.calibration-values.txt) \
  | bc

rm 1.calibration-values.txt 1.second.txt 1.first.txt
