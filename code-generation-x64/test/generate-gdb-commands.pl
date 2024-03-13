#!/usr/bin/env perl

use strict;
use warnings;

while (<>) {
    my $i = index($_, "DEBUGGER: ");

    if ($i >= 0) {
        print substr($_, $i + length("DEBUGGER: "));
    }
}
