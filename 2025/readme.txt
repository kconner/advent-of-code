brew install rakudo-star

run:
raku 1.raku

watch:
ls -1 | entr -c raku 1.raku
