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

# problem 1

my $invalid-id-sum = 0;

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

  # for each subrange whose digit count is even,
  for %subranges-by-digit-count.pairs.grep(*.key %% 2) -> $pair {
    my $digit-count = $pair.key();
    my $subrange = $pair.value();

    # i want to know how many multiples of 11, 101, 1001, etc. exist in the subrange
    # 2 -> 10, 4 -> 100, 6 -> 1000
    my $half-digit-count = $digit-count +> 1;
    my $split-factor = 10**$half-digit-count;
    my $invalid-divisor = $split-factor + 1;
    
    # accumulate numbers in the range that are divisible by the invalid divisor.
    # my $invalid-number-count = $subrange.grep(* %% $invalid-divisor).elems;
    for $subrange.grep(* %% $invalid-divisor) -> $id {
      $invalid-id-sum += $id;
    }
  }
}

say $invalid-id-sum;
