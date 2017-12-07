#!/usr/bin/perl

my @myinput = [0];
my $d = 0;
my $n = 0;
my $pos = 0;
my $len = 0;

while ($i = <>) {
    my $j = chomp $i;
#    print "*$i:$d";
    $myinput[$d] = $i;
#    print "*$myinput[$d]";
    $d++;
}

#for $a (@myinput) {
#    print $a;
#}

$len = $d;
    
#print "% $myinput[0] % $myinput[1] % $myinput[2] % $myinput[3] % $myinput[4] %%";
#print "%% @myinput[0] %% @myinput[1] %% @myinput[2] %% @myinput[3] %% @myinput[4] %%";

while ($pos >= 0 && $pos < $len){
#    print  "/$pos~$myinput[$pos]&$n";
    my $pos1 = $pos + $myinput[$pos];
    if ($myinput[$pos] >= 3){
#	print "-";
	$myinput[$pos] -= 1;
    }else{
#	print "+";
	$myinput[$pos] += 1;
    }
    $n++;
    $pos = $pos1;
}
print $n;
