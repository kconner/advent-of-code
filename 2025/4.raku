# for each cell that is a '@',
# sample the 8 adjacent cells
# count those that are '@'
# if that's < 4, it's forklift acessible
# count the forklift accessible cells

my @rows = slurp('4.txt').lines>>.comb;

my %grid;
for 0..^@rows.elems -> $r {
  for 0..^@rows[0].elems -> $c {
    if @rows[$r][$c] eq '@' {
      %grid{"$r,$c"} = ($r, $c);
    }
  }
}

# problem 1

my $accessible-count = 0;
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
    $accessible-count += 1;
  }
}

say $accessible-count;
