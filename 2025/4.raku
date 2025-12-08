my @rows = slurp('4.txt').lines>>.comb;

my %grid;
for 0..^@rows.elems -> $r {
  for 0..^@rows[0].elems -> $c {
    if @rows[$r][$c] eq '@' {
      %grid{"$r,$c"} = ($r, $c);
    }
  }
}

sub accessible-locations() {
  gather {
    for %grid.values -> ($r, $c) {
      my $ru = $r - 1;
      my $rd = $r + 1;
      my $cl = $c - 1;
      my $cr = $c + 1;
      my $neighbor-count = %grid{
        "$ru,$cl", "$ru,$c", "$ru,$cr",
        "$r,$cl", "$r,$cr",
        "$rd,$cl", "$rd,$c", "$rd,$cr"
      }:exists.sum;
      
      if $neighbor-count < 4 {
        take "$r,$c";
      }
    }
  }
}

# problem 1

say accessible-locations().elems;

# problem 2

my $accessible-count = 0;
loop {
  my @locations = accessible-locations();
  last unless @locations;
  $accessible-count += @locations.elems;
  %grid{@locations}:delete;
}

say $accessible-count;
