package MasonX::Plugin::UTF8;

use base qw(HTML::Mason::Plugin);
use warnings;
use strict;

sub start_request_hook {
    # convert mason args from UTF-8 to internal
    my ( $self, $context ) = @_;
    my $args_ref = $context->args();
    foreach my $arg ( @{$args_ref} ) {
        utf8::is_utf8($arg) || utf8::decode($arg);
    }
    # convert ENV from UTF-8 to internal
    foreach (keys %ENV) {
        utf8::is_utf8($ENV{$_}) || utf8::decode($ENV{$_});
    }

    return;
}

1;
