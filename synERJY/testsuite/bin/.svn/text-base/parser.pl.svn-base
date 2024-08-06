$debug = 0;

print "checking yacc and lex consistency...\n";

$c = "cat \$SE_HOME/src/*.mll |";
$debug && print "executing now: $c\n";
open(IN,$c);
@lex = <IN>;
close(IN);
$lexl = scalar(@lex);
$debug && print "operating on $lexl lines of mll-source.\n";

$file = "cat \$SE_HOME/src/*.mly |";

open(IN,$file);
@yacc = <IN>;
close(IN);
$yaccl = scalar(@yacc);

$debug && print "$yaccl lines read from $file\n";

$debug && print "checking for unused tokens\n";
$y = 0;
while ($y < $yaccl) {
  $l = $yacc[$y];
  chop($l);
  $y++;
  if ($l =~ /^%TOKEN\s*(\<.*\>)?\s+(.*)$/) {
     $t  = $2;
     $ok = 0;
     $debug && print "checking $t\n";
     $l = $y + 1;
     while ($l < $yaccl) {
       if ($yacc[$l] =~ /\s+$t\s+/) {
          $ok = true;
          last;
       };
       $l++;
     };
     if ($ok) {
     } else {
        print "FAULT: token $t is unused in the yacc source\n";
     };
     $l = 0;
     while ($l < $lexl) {
       if ($lex[$l] =~ /\s+$t\s+/) {
          $ok = true;
          last;
       };
       $l++;
     };
     if ($ok) {
     } else {
        print "FAULT: token $t is unused in the lex source\n";
     };
  } elsif ($yacc[$l] =~ /[tT][oO][kK][eE][nN]/) {
     print "FAULT: line \"$yacc[$l]\" ($l) looks suspicious\n";
  };
};

$debug && print "checking for invalid nonterminals\n";
%nonterminals = ();
$y = 0;
while ($y < $yaccl) { # skip irrelevant def's
  $l = $yacc[$y];
  $y++;
  if ($l =~ /^\/\* ---- START OF NONTERMINALS -----/) {
     last;
  };
};

while ($y < $yaccl) {
  $l = $yacc[$y];
  $y++;
  if ($l =~ /^([ABCDEFGHIJKLMNOPQRSTUVWXYZ]\S+):$/) {
     if ($nonterminals{$1}) {
        print "FAULT: mult-def of $1 in line $y\n";
     };
     if ($yacc[$y] =~ /^\s*\|/) {
        print "FAULT: suspicious empty rule in $1 in line $y\n";
     };
     if ($l =~ /_/) {
        print "FAULT: suspicious nonterminal name in $1 in line $y\n";
     };
     $nonterminals{$1} = 1;
     $debug && print "nonterminal $1\n";
  } elsif ($l =~ /::/) {
     # ignore cons
  } elsif ($l =~ /:/) {
     print "FAULT: suspicious colon in line $y: $l\n";
  };
};

$y = 0;
while ($y < $yaccl) {
  $l = $yacc[$y];
  $y++;
  if ($l =~ /^([ABCDEFGHIJKLMNOPQRSTUVWXYZ]\S+):$/) {
     # ignore definitions
  } elsif ($l =~ /^\S+:\s+(\S+)\s/) {
     # start rules like "prs_rule: NonTerminal { $1 }"
     $n = $1;
     if ($nonterminals{$n}) {
        $nonterminals{$n} = $nonterminals{$n} + 1;
     };
  } else {
     foreach $n (split(/\s/,$l)) {
       if ($nonterminals{$n}) {
          $nonterminals{$n} = $nonterminals{$n} + 1;
       };
     };
  };
};

foreach $n (keys(%nonterminals)) {
  $c = $nonterminals{$n} - 1;
  if ($c == 0) {
     print "FAULT: nonterminal unused: $n\n";
  } else {
     $debug && print "nonterminal $n used $c times\n";
  };
};
