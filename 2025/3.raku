my @banks = slurp('3.txt').lines;

sub jolterinos($digit-count) {
  my @chops = reverse 0..^$digit-count;

  say [+] gather {
    for @banks -> $bank {
      my $bank-tail = $bank;
      
      # to get the n-digit joltage for a bank we need to find the first position
      # where the highest digit of the substring, excluding enough digits to
      # complete it, is found. then take the substring after that to the end,
      # and repeat. stitch the digits.
      take [~] gather {
        for @chops -> $chop-depth {
          my $max-digit = chop($bank-tail, $chop-depth).comb.max;
          take $max-digit;
          $bank-tail = $bank-tail.substr($bank-tail.index($max-digit) + 1);
        }
      }
    }
  }
}

# problem 1
jolterinos(2);

# problem 2
jolterinos(12);
