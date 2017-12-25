#!/usr/bin/perl

my %nextState = ( "A0" => 'B', "A1" => 'C', "B0" => 'A', "B1" => 'D', "C0" => 'B', "C1" => 'E', "D0" => 'A', "D1" => 'B', "E0" => 'F', "E1" => 'C', "F0" => 'D', "F1" => 'A');
my %output = ( "A0" => 1, "A1" => 0, "B0" => 1, "B1" => 1, "C0" => 0, "C1" => 0, "D0" => 1, "D1" => 0, "E0" => 1, "E1" => 1, "F0" => 1, "F1" => 1);
my %direction = ( "A0" => 1, "A1" => -1, "B0" => -1, "B1" => 1, "C0" => -1, "C1" => -1, "D0" => 1, "D1" => 1, "E0" => -1, "E1" => -1, "F0" => 1, "F1" => 1);

my %tape;

my $state='A';
my $pos=0;

my $count = 12667664;

while ($count != 0){
    $val = $tape{$pos} || 0;
#    print "s: $state p: $pos v: $val ";
    $key = "$state$val";
    $state = $nextState{$key};
    $tape{$pos} = $output{$key};
    $pos += $direction{$key};
#    print "k: $key ns: $state np: $pos o: $tape{$pos}\n";
    $count--;
}

for my $bit (keys %tape){
    $count+=$tape{$bit};
#    print $bit;
}

print "\n$count\n";
