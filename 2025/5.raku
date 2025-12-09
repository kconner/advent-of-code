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

sub union($ra, $rb) {
  $ra.min..max($ra.max, $rb.max);
}

my @index-set-ranges;
sub add-range($range) {
  if @index-set-ranges && $range.min <= @index-set-ranges[*-1].max {
    @index-set-ranges[*-1] = union(@index-set-ranges[*-1], $range);
  } else {
    @index-set-ranges.push($range);
  }
}

for sort $fresh-ranges -> $range {
  add-range($range);
}

sub is-fresh($id) {
  ? @index-set-ranges.first($id ~~ *)
}

# problem 1
say $ids.grep({is-fresh($_)}).elems;

# problem 2
say [+] @index-set-ranges>>.elems