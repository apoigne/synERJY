#!/usr/bin/perl
# this is a perl script checking whether a eps file is used in tex-sources
# usage is: perl eps_used.pl
# the following two variables define the directories
# - where the eps files are stored
# - where the directories are stored, which contain the spources of the
#   sE documentation [all tex-files are then: $tex_dir/*/*.tex $tex_dir/*.tex

$eps_dir = '$SE_HOME/doc/eps';
$tex_dir = '$SE_HOME/doc';

open(IN,"cd $eps_dir;ls *eps* |");
@files = <IN>;
close(IN);
foreach $f (@files) {
  chop($f);
};
$len = @files;
print "eps[f] files to consider: $len\n";

open(IN,"cat $tex_dir/*/*.tex $tex_dir/*.tex |");
@tex = <IN>;
close(IN);

$len = @tex;
print "lines of tex to analyze:  $len\n";

foreach $f (@files) {
  $used = 0;
  foreach $t (@tex) {
    if ($t =~ /$f/) {
       $used++;
    };
  };
  if ($used == 0) {
     print "XXX NOT USED: $f\n";
  } elsif ($used == 1) {
     printf("  1 usage  of  %s\n",$f);
  } else {
     printf("%3d usages of %s\n",$used,$f);
  };
};
