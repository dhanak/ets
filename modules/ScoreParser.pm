package ScoreParser;

use Carp;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(&parseScores);

# Line format:
# NEPTUN:P1:P2:P3:P4:lang=LNG,group=GRP,corr=CORR % comment

# Parse scores from handle passed as an argument
sub parseScores(*) {
    my @allScores;
    my $fh = shift;

    while (<$fh>) {
	chomp;			# strip whitespaces
	s/ *%.*$//;		# strip line end komments
	next if /^$/;		# skip if line is empty

	my ($neptun,$p1,$p2,$p3,$p4,$rems) = split /:/, uc $_;
	my @scores = ($p1,$p2,$p3,$p4);
	my ($Lang,$Group,$Corr) = split /,/, $rems;
	my ($n1,$lang) = split /=/, $Lang;
	my ($n2,$group) = split /=/, $Group;
	my ($n3,$corr) = split /=/, $Corr;

	if ($#scores != 3) {	# this doesn't seem to be a score line
	    carp "Suspicious looking line $. skipped.\n";
	    next;
	}

	push @allScores, { neptun => $neptun,
			   group => $group,
			   language => $lang,
			   scores => \@scores,
			   corrector => $corr };
    }

    return @allScores;
}

1;
