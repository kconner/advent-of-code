grammar Input {
  token TOP { <range> [ \n <range> ]* \n\n <id> [ \n <id> ]* \n? }
  token range { <low> '-' <high> }
  token low { \d+ }
  token high { \d+ }
  token id { \d+ }
}

class InputActions {
  method TOP($/) {
    make [ $<range>>>.made, $<id>>>.Int ]
  }
  method range($/) {
    make $<low>..$<high>
  }
}

my $input = slurp('5.txt');
my ($fresh-ranges, $ids) = Input.parse($input, actions => InputActions).made;

sub is-fresh($id) {
  ? $fresh-ranges.first($id ~~ *)
}

# problem 1

say $ids.grep({is-fresh($_)}).elems;
