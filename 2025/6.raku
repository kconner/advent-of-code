grammar Input {
  token TOP { <number-line>+ <operator-line> }
  token number-line { \s* [ <number> ' '* ]+ \n }
  token number { \d+ }
  token operator-line { \s* [ <operator> ' '* ]+ \n? }
  token operator { '+' | '*' }
}

class InputActions {
  method TOP($/) {
    make $<number-line>>>.made.push($<operator-line>.made);
  }
  method number-line($/) {
    make $<number>>>.made
  }
  method number($/) {
    make +$/
  }
  method operator-line($/) {
    make $<operator>>>.made
  }
  method operator($/) {
    make "$/"
  }
}

my $input = slurp('6.txt');
my @lines = Input.parse($input, actions => InputActions).made;
my @problems = zip(|@lines>>.list.rotate(-1));

# problem 1

say [+] gather {
  for @problems -> $problem {
    my ($operator, @operands) = $problem;
    given $operator {
      when "*" { take [*] @operands; }
      when "+" { take [+] @operands; }
    }
  }
}