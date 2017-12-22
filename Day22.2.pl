#!/usr/bin/perl
use Switch

my %world;
my @tempworld;

my $x;
my $y;

my $thestate;

$y=0;

while (<>){
    $i=0;
    while ($i<25){
	$thestate=substr ($_, $i, 1);
	$tempworld[$y][$i] = $thestate;
	$i++;
    }
    $y++;
};

$y=0;

while ($y<25){
    $x=0;
    while ($x<25){
	$key="$x:$y";
	$world{$key}=$tempworld[$y][$x];
	$x++;
    };
    $y=$y+1;
};

my $antx=12;
my $anty=12;
my $antdir=0;
my $antinf=0;

my $count=10000000;

while ($count>0){
    my $state = $world{"$antx:$anty"} || '.';

    switch ($state) {
	case '#' {
	    $antdir=($antdir+1) % 4;
	    $world{"$antx:$anty"} = 'F';
	};
	case '.' { 
	    $antdir=($antdir-1) % 4;
	    $world{"$antx:$anty"} = 'W';
	};
	case 'F' {
	    $antdir = ($antdir+2) % 4;
	    $world{"$antx:$anty"} = '.';
	};
	case 'W' {
	    $world{"$antx:$anty"} = '#';
	    $antinf++;
	};
    };
    switch ($antdir){
	case 0 {
	    $anty--;
	};
	case 1 {
	    $antx++;
	};
	case 2 {
	    $anty++;
	};
	case 3 {
	    $antx--;
	};
    };
    $count--;
};

print $antinf;
