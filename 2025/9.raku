my @spots = slurp('9.txt').lines>>.split(',');

# problem 1

sub area($sa, $sb) {
  (1 + abs($sa[0] - $sb[0])) * (1 + abs($sa[1] - $sb[1]));
}

say @spots.combinations(2).map({area(|$_)}).sort[*-1];
