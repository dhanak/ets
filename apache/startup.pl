#!/bin/perl -w

package HTML::Mason::Commands;

use lib qw(modules);

use DBD::MariaDB;
use Data::Dumper;
use Text::Wrap;
use URI::Escape;

use vars qw($data $dbh $maintenance $sid);

$Data::Dumper::Indent = 0;
$Data::Dumper::Purity = 1;

package HTML::Mason;

# Increase shared memory by preloading CGI.pm (also works without this)
use CGI;

sub childinit {
    $dbh = undef;
    $SIG{CHLD} = sub { wait; };
}

1;
