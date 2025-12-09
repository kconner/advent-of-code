my $input = slurp('6.txt');

sub sum-answers(@problems) {
  say [+] gather {
    for @problems -> $problem {
      my ($operator, @operands) = $problem;
      given $operator {
        when "*" { take [*] @operands; }
        when "+" { take [+] @operands; }
      }
    }
  }
}

# problem 1

grammar Input1 {
  token TOP { <number-line>+ <operator-line> }
  token number-line { ' '* [ <number> ' '* ]+ \n }
  token number { \d+ }
  token operator-line { ' '* [ <operator> ' '* ]+ \n? }
  token operator { '+' | '*' }
}

class Input1Actions {
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
    make ~$/
  }
}

my @lines = Input1.parse($input, actions => Input1Actions).made;
my @problems1 = zip(|@lines>>.list.rotate(-1));

sum-answers(@problems1);

# problem 2

grammar Input2 {
  token TOP { <problem> [ <blank-line> <problem> ]* }
  token problem { [ ' '* <number> ' '* \n ]* ' '* <number> ' '* <operator> \n }
  token number { \d+ }
  token operator { '+' | '*' }
  token blank-line { ' '+ \n }
}

class Input2Actions {
  method TOP($/) {
    make $<problem>>>.made
  }
  method problem($/) {
    make ($<operator>.made, |$<number>>>.made)
  }
  method number($/) {
    make +$/
  }
  method operator($/) {
    make ~$/
  }
}

my $input2 = zip(|$input.lines>>.comb)>>.join.reverse.join("\n");
my @problems2 = Input2.parse("$input2\n", actions => Input2Actions).made;

sum-answers(@problems2);
