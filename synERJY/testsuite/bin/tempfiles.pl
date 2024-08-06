$debug = 0;

print "checking for temporary files\n";

(require "$ENV{HOME}/.setestrc")
   || die "unable to read $ENV{'HOME'}/.setestrc";

$c = "   find \$SE_HOME -print | fgrep \'/se.\' | ";
$c = "$c fgrep -v \'/doc/tex_input/\' | fgrep -v \'/bin/se.bat\' |";
$c = "$c fgrep -v \'/misc/se.vim\' | fgrep -v \'/images/se.gif\' |";

$debug && print "executing now: $c\n";
open(IN,$c);
@f = <IN>;
close(IN);

$length = @f;
if ($length == 0) {
   print "no temporary files found, OK\n";
} else {
   foreach $file (@f) {
     print "file: $file";
   };
   print "$SIG temporary files found\n";
   print "$SIG $FLT\n";
}

exit(0);
