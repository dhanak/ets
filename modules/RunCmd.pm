package RunCmd;

use File::Temp qw(tempfile);
use Carp;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(&runcmd);

sub runcmd($$)
{
    my ($cmd, $input) = @_;
    my ($output, $error);
    my ($ofile, $efile) = (File::Temp->new(), File::Temp->new());

    print STDERR "--- COMMAND ---\n$cmd\n--- STDIN ---\n$input\n";
    open IN, "| $cmd >$ofile 2>$efile" or croak "runcmd failed: $cmd: $!";
    print IN $input;
    close IN;

    open OUT, "<$ofile" or croak "runcmd failed: $ofile: $!";
    $output = join "", <OUT>;
    close OUT;
    print STDERR "--- STDOUT ---\n$output\n";

    open ERR, "<$efile" or croak "runcmd failed: $efile: $!";
    $error = join "", <ERR>;
    close ERR;
    print STDERR "--- STDERR ---\n$error\n";

    return ($output, $error);
}

1;
