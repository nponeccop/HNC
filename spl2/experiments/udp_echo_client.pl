use strict;
use warnings;
use IO::Socket::INET;
use Time::HiRes qw(sleep);
use Benchmark;

my $r = '173.203.79.183';

my $sock = IO::Socket::INET->new(PeerAddr => '127.0.0.1',
                                 PeerPort => '99',
				 LocalPort => '7777',
                                 Proto    => 'udp',
				Timeout => 2) or die;

timethis 10000,
sub {

	my $x = 'a' x (1472);
	$sock->send($x);
	my $y = 'foo';
	$sock->recv($y, 2000);
};
