#!/usr/local/bin/perl5

$infile  = "se.default_in.tr";
$outfile = "se.default_out.tr";

print "make regression trace files from simulator trace files\n";
print "trace.pl infile outfile\n";

$p = shift(@ARGV);
if ($p eq '' or $p =~ /^-/) {
   die "invalid input file name: $p";
} else {
   $infile = $p;
};
$p = shift(@ARGV);
if ($p eq '' or $p =~ /^-/) {
   die "invalid output file name: $p";
} else {
   $outfile = $p;
};
$p = shift(@ARGV);
if ($p ne '') {
   die "invalid thirs parameter: $p";
};

open(IN, "<$infile");
open(OUT,">$outfile");

while (1) {
  $l = <IN> || die "invalid trace file header";
  chop($l);
  if ($l =~ /^}$/) { last; };
};

print "skipped header\n";
$i = 0;
while (1) {
  $i++;
  print "instant $i\n";
  $c = '';
  while (1) {
    $eof = 0;
    $l = <IN> || ( $eof = 1 );
    if ($eof and $c == '') {
       close(IN); close(OUT); print "finished\n"; exit(0);
    } elsif ($eof) {
       die "invalid instant data {+]: EOF";
    };
    chop($l);
    if ($l =~ /^\+\+i/)  { last; };
    if ($l =~ /^\+(\S+)\s*\(([^\)]*)\)\s*$/) {
       if ($2 eq '') {
          $c = "$c$1 ";
       } else {
          $c = "$c$1($2) ";
       }
    } else {
       die "invalid instant data [+] found: $l\n";
    };
  };
  if ($c eq '') {
     $c = ' ->';
  } else {
     $c = "$c->";
  };
  while (1) {
    $l = <IN> || die "invalid instant data {-]: EOF";
    chop($l);
    if ($l =~ /^--i/) { last; };
    if ($l =~ /^-(\S+)\s*\(([^\)]*)\)\s*$/) {
       $n = $1;
       $p = $2;
       if ($p eq '') {
          $c = "$c $n";
       } else {
          $c = "$c $n($p)";
       }
    } else {
       die "invalid instant data [-] found: $l\n";
    };
  };
  print OUT "// $c\n";
};

exit(0);
