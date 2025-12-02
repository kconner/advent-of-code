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
my $zero-stop-count = 0;

for @items -> $clicks-to-right {
  $dial-point = ($dial-point + $clicks-to-right) % 100;

  if $dial-point == 0 {
    $zero-stop-count += 1;
  }
}

say $zero-stop-count;

# problem 2

$dial-point = 50;
my $zero-visit-count = 0;

for @items -> $clicks-to-right {
  my $trivial-next-point = $dial-point + $clicks-to-right;
  given $clicks-to-right {
    when 0 < * {
      if $trivial-next-point < 100 {
        $dial-point = $trivial-next-point;
      } else {
        my $remaining-clicks-to-right = $clicks-to-right - (100 - $dial-point);
        $dial-point = 100;
        $zero-visit-count += 1 + $remaining-clicks-to-right div 100;
        $dial-point = ($dial-point + $remaining-clicks-to-right) % 100;
      }
    }
    when * < 0 {
      if 0 < $trivial-next-point {
        $dial-point = $trivial-next-point;
      } else {
        # Count the first wrap as a zero visit only if we didn't start on zero
        if 0 < $dial-point {
          $zero-visit-count += 1;
        }

        my $remaining-clicks-to-right = $clicks-to-right + $dial-point;
        $dial-point = 0;
        $zero-visit-count += $remaining-clicks-to-right div -100;
        $dial-point = ($dial-point + $remaining-clicks-to-right) % 100;
      }
    }
  }
}

say $zero-visit-count;
