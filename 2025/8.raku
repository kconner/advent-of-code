my @boxen = slurp('8.txt').lines>>.split(',');

my $join-count = 1000; # 10 for test data

sub square-distance($a, $b) {
  ($a[0] - $b[0]) ** 2 + ($a[1] - $b[1]) ** 2 + ($a[2] - $b[2]) ** 2;
}

my %circuits-by-box;
my %boxen-by-circuit;
for @boxen.keys -> $box-index {
  my $circuit-id = $box-index;
  %circuits-by-box{$box-index} = $circuit-id;
  %boxen-by-circuit{$circuit-id} = [$box-index];
}

# reassign circuit b boxes to a
sub merge($ca, $cb) {
  return if $ca eq $cb;

  for %boxen-by-circuit{$cb}.list -> $box-id {
    %circuits-by-box{$box-id} = $ca;
  }

  %boxen-by-circuit{$ca}.push(|%boxen-by-circuit{$cb});
  %boxen-by-circuit{$cb}:delete;
}

# problem 1

# for all pairs of boxen, gather distances and index pairs,
# order by distance ascending, take the first $join-count, and merge pairs.
# get the product of the 3 highest box counts.

for @boxen.keys.combinations(2).map({square-distance(|$_.map({@boxen[$_]})), $_}).sort.map({$_[1]})[0..^$join-count] -> $pair {
  merge(|$pair.map({%circuits-by-box{$_}}));
}

say [*] %boxen-by-circuit.values>>.elems.sort.reverse[0..^3];