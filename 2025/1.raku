grammar Input {
  token TOP { [ <item> \n ]+ }
  token item { <letter> <number> }
  token letter { 'L' | 'R' }
  token number { \d+ }
}

class InputActions {
  method TOP($/) { make $<item>>>.made }
  method item($/) {
    given $<letter> {
      when 'L' { make -$<number> }
      when 'R' { make +$<number> }
    }
  }
}

my $input = slurp('1.txt');
my @items = Input.parse($input, actions => InputActions).made;

# problem 1

my $dial-point = 50;
my $zero-count = 0;

sub turn-dial($clicks-to-right) {
  $dial-point = ($dial-point + $clicks-to-right) % 100;
}

for @items -> $clicks-to-right {
  turn-dial($clicks-to-right);
  if $dial-point == 0 {
    $zero-count += 1;
  }
}

say $zero-count;
