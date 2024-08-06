$debug = 0;

$c = '(cd $SE_HOME;ls src/*.ml builtin/*.ml) |';
$debug && print "executing now: $c\n";
open(IN,$c);
@f = <IN>;
close(IN);

$c = '';
$i = 0;
$length = scalar(@f);
while ($i < $length) {
  chop($f[$i]);
  $l = $f[$i];
  if ($l =~ /yacc\.ml/ or $l =~ /lex\.ml/ or $l =~ /builtin_data\.ml/ or
      $l =~ /err\.ml/) {
  } else {
     $c = "$c$l ";
  };
  $i++;
};

$c = "(cd \$SE_HOME;cat $c src/*.mly src/*.mll) |";
$debug && print "executing now: $c\n";
open(IN,$c);
@f = <IN>;
close(IN);
$flength = scalar(@f);
print "operating on $flength lines of sE-source.\n";

$file = "cat \$SE_HOME/src/err.ml |";

open(IN,$file);
@d = <IN>;
close(IN);
$length = scalar(@d);

$debug && print "$length lines read from $file\n";

$i = 0;
$s = 0;
$e = 0;
while ($i < $length) {
  chop($d[$i]);
  if ($d[$i] =~ /^\(\* start of type declaration for error messages \*\)$/) {
     $s = $i;
  };
  if ($d[$i] =~ /^\(\* end of type declaration for error messages \*\)$/) {
     $e = $i;
  };
  $i++;
};

if ($s == 0 or $e == 0) {
   print "ERROR: start or end of error type declaration not found.\n";
   exit(1);
};

$s++;
print "type declarations for error messages start at $s and terminate at $e\n";

while ($s < $e) {
  if ($d[$s] =~ /\s*\|\s+(\w+)/) {
     $err = $1;
     $ok  = 0;
     $i   = $s;
     while ($i < $length) {
       if ($d[$i] =~ /^\| $err.*\[\[$err\]\]/) {
          $ok = 1;
       };
       $i++;
     };
     if (not $ok) {
        print "ERROR: no definition found which matches $err\n";
     };
     $ok  = 0;
     $i   = $0;
     while ($i < $flength) {
       if ($f[$i] =~ /Err\.$err/) {
          $ok = 1;
          last;
       };
       $i++;
     };
     if (not $ok) {
        print "ERROR: no use found for error $err\n";
     };
  } else {
     print "ERROR: illegal error type declaration found: $d[$s]\n";
  };
  $s++;
}

exit(0);
