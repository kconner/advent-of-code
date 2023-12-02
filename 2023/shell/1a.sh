#!/bin/bash -euo pipefail

cat 1.txt | sed 's/[^0-9]//g' | cut -c1 > 1.first.txt
cat 1.txt | rev | sed 's/[^0-9]//g' | cut -c1 > 1.second.txt
paste -d'\0' 1.first.txt 1.second.txt > 1.calibration-values.txt 
paste -s -d'+' 1.calibration-values.txt | bc
rm 1.calibration-values.txt 1.second.txt 1.first.txt
