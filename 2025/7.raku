# get all the lines
# break them into first and rest
# with the first line, find the first index matching S
# initialize the beam index set with that index
# initialize the split count with zero
# for each of the rest of the lines,
# for each beam index in the set, if the line's character at that index is ^,
# count a new split
# remove the index from the beam set
# add the two adjacent indices to the beam set
# say the split count

my @lines = slurp('7.txt').lines;
my ($first-line, @rest-of-lines) = |@lines;

my %beam-index-set;
%beam-index-set{$first-line.index('S')} = True;

my $split-count = 0;

for @rest-of-lines -> $line {
  my @keys = %beam-index-set.keys;
  for @keys -> $index {
    if $line.comb[$index] eq '^' {
      $split-count += 1;
      %beam-index-set{$index}:delete;
      %beam-index-set{$index - 1} = True;
      %beam-index-set{$index + 1} = True;
    }
  }
}

say $split-count;