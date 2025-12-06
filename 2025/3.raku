my @banks = slurp('3.txt').lines;

# problem 1;

say [+] gather {
  for @banks -> $bank {
    # to get the joltage for a line we need to find the first position where the
    # highest digit of the substring excluding the last digit is found.
    # then take the substring after that to the end, and find the highest digit.
    # stitch those two and that's the value.
    my $first-digit = chop($bank).comb.max;
    my $bank-tail = $bank.substr($bank.index($first-digit) + 1);
    my $second-digit = $bank-tail.comb.max;
    take $first-digit ~ $second-digit;
  }
}
