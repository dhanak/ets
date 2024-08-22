package ScoreParser;

use Carp;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(&parseScores);

# Line format:
# NEPTUN:P1:P2:P3:...:group=GRP,corr=CORR % comment

# Parse scores from handle passed as an argument
sub parseScores(*) {
    my @allScores;
    my $fh = shift;

    while (<$fh>) {
        chomp;                  # strip white spaces
        s/ *%.*$//;             # strip line end comments
        next if /^$/;           # skip if line is empty

        my @fields = split /:/, uc $_;
        my $neptun = $fields[0];
        my @scores = @fields[1..$#fields-1];
        my %notes = ();
        foreach my $part (split /,/, $fields[$#fields]) {
            my ($label, $value) = split /=/, $part;
            $notes{$label} = $value;
        }
        push @allScores, {
            neptun => $neptun,
            notes  => \%notes,
            scores => \@scores,
        };
    }

    return @allScores;
}

1;
