grammar Input {
  token TOP { <range> [ ',' <range> ]* \n? }
  token range { <low> '-' <high> }
  token low { \d+ }
  token high { \d+ }
}

class InputActions {
  method TOP($/) { make $<range>>>.made }
  method range($/) {
    make $<low>..$<high>
  }
}

my $input = slurp('2.txt');
my @ranges = Input.parse($input, actions => InputActions).made;
say @ranges;

# problem 1

# get all the ranges
# init the sum of invalid product ids at zero
my $invalid-id-sum = 0;

# for each range:
for @ranges -> $range {
  my $remaining-range = $range;

  # break the range into subranges by digit count
  my %subranges-by-digit-count;
  while $remaining-range.min.chars < $remaining-range.max.chars {
    my $digit-count = $remaining-range.min.chars;
    my $next-lower = 10**$digit-count;
    %subranges-by-digit-count{$digit-count} = $remaining-range.min..($next-lower - 1);
    $remaining-range = $next-lower..$remaining-range.max;
  }
  %subranges-by-digit-count{$remaining-range.min.chars} = $remaining-range;
  
  say %subranges-by-digit-count;

  # for each subrange whose digit count is even,
  for %subranges-by-digit-count.pairs.grep(*.key %% 2) -> $pair {
    my $digit-count = $pair.key();
    my $subrange = $pair.value();
    # say $digit-count;
    # say $subrange;

    # i want to know how many multiples of 11, 101, 1001, etc. exist in the subrange
    # 2 -> 10, 4 -> 100, 6 -> 1000
    my $half-digit-count = $digit-count +> 1;
    my $split-factor = 10**$half-digit-count;
    my $invalid-divisor = $split-factor + 1;

    # first narrow the search by the intersection of ranges of the first and second halves
    # aaabbb-cccddd -> aaa..ccc bbb..ddd -> bbb..ccc
    my $left-digits-range = ($subrange.min div $split-factor)..($subrange.max div $split-factor);
    my $right-digits-range = ($subrange.min % $split-factor)..($subrange.max % $split-factor);
    # say $left-digits-range;
    # say $right-digits-range;
    
    my $intersection-range = max($left-digits-range.min, $right-digits-range.min)..min($left-digits-range.max, $right-digits-range.max);
    say $intersection-range;
    say $intersection-range.elems;
    
    # i don't think this is right, because for example 6077..9999 should allow 6161 but intersecting constrains to 77..99 per half

    # my $invalid-number-count = $intersection-range.grep(* %% $invalid-divisor).elems;
    for $intersection-range.list -> $half-invalid-id {
      $invalid-id-sum += $half-invalid-id + $half-invalid-id * $split-factor;
    }
  }

}

say $invalid-id-sum;
