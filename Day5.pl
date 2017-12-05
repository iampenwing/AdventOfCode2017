#!/usr/bin/perl

@inp = ();
print @inp;

while ($i = <>) {
    push @inp, chomp($i);
}

$n = 0;
$pos = 0;
$len = @inp;

print "** $n ** $pos ** $inp[$pos] ** $len **";

while ($pos >= 0 && $pos < $len){
    if ($inp[$pos] >= 3){
	$inp[$pos] = $inp[$pos]--;
    }else{
	$inp[$pos] = $inp[$pos]++;
    }
    $n++
}
print $n;
