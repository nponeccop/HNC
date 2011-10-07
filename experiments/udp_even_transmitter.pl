use strict;
use warnings;
use IO::Socket::INET;
use Time::HiRes qw(sleep);
use Benchmark;
use Event;
use Data::Dumper;
my $r = '173.203.79.183';
my $g = '92.113.74.149';
my $ll = '127.0.0.1';

my $interval = 0.5;

# max = 1472
my $size = 1472;

my $sock = IO::Socket::INET->new(PeerAddr => $ll,
                                 PeerPort => '99',
				 LocalPort => '7777',
                                 Proto    => 'udp') or die;
my $x = 'a' x $size;
my @sendQueue;

my $sentCount = 0;
my $receivedCount = 0;


sub onRead
{
	my $y = 'foo';
	$sock->recv($y, 2000);
	if (length($y) > 0)
	{
		$receivedCount += 1;
		my $xx = int(100 - $receivedCount * 100 / $sentCount);
		my $lost = $sentCount - $receivedCount;
		print "Sent = $sentCount; Lost = $lost ($xx% loss)\n";
#		print "Received = $y\n";
#		push @sendQueue, $x;
	}
}

sub onWrite
{
	if (scalar(@sendQueue) > 0)
	{
		$sock->send(shift @sendQueue);	
	}
	else
	{
		$sock->send($x);
	}
	$sentCount += 1;
	print "Sent = $sentCount\n";
}                         

my $w = Event->io(fd => $sock, poll => 'r', cb => \&onRead);

my $ww = Event->timer(interval => $interval, after => 0, cb => \&onWrite);

my $l = Event::loop();
