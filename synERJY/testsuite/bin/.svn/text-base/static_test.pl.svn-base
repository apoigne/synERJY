# purpose: apply static test-file (optionally only for one test case)
sub test {

if ($case eq '') { # really: sta_file #########################################
   $msg = "static test file $file";
} else { #         # really: sta_case #########################################
   $msg = "static test file $file test case $case";
};
log_ok ($msg);

$dlevel_O = 'None';     # no debug level needed ... for O<n> cases
$appl_O   = 'false';    # no application must be generated ... ...
$dlevel_E = 'None';     # no debug level needed ... for E<n> cases
$appl_E   = 'false';    # no application must be generated ... ...

$confclass = '';        # configuration class

$testflagspat = '\s*=\s*(\w+)\s+(\w+)\s*';
$testflagspat = '\/\/\s*test-flags\s+O'.$testflagspat.',\s*E'.$testflagspat;
$testflagspat = $testflagspat.'(,\s*confclass\s+(\w*)|)';

open (IN,"<$file");
   while ($line = <IN>) {
      if ( $line =~ /^#ifdef ([OE])(\d*)/ ) {
         last;
      } elsif ( $line =~ /^$testflagspat$/ ) {
         $dlevel_O = $1;
         $appl_O   = $2;
         $dlevel_E = $3;
         $appl_E   = $4;
         if ($5 ne $confclass) {
            $confclass = $6;
         };         
         last;
      } elsif ( $line =~ /test-flags/ ) {
         log_exit ("test-flag setting invalid");
      };
   };
close (IN);

####### the implicit or explicit O1 test-case #################################
if ($case eq '' or $case eq 'O1') {
   system ("$CPP -I$SE_HOME/Examples -DO1 $file > $SEF");
   system ("cp $SEF se.sta.O1");
   $cmd = "$SE_HOME/testsuite/bin/se_run_cmp";
   $cmd = "($cmd $SEF $SE_HOME $dlevel_O $appl_O $SE_COMP $confclass 2>&1) |";
   open (SE_TC,$cmd);
   &check_run;
   @se_out = <SE_TC>;
   close (SE_TC);

   &log_ok ("testing O1");
   $errors = 0;
   $fatale = 0;
   foreach $line (@se_out) { &check_errors () };
   if ($errors > 0 or $fatale > 0 or $verb_level==2) {
      foreach $line (@se_out) { print $line };
   };
   if ($errors > 0) {
      log_err ("$errors error in O1");
   } elsif ($fatale > 0) {
      log_err ("$fatale FATALE error in O1");
   } else {
      log_ok ("case O1 ok");
   };
};

if ($case eq 'O1') { exit (0); };

####### the explicit O<i> [i >= 2] or explicit E<j> [j >= 1] test-cases #######
open (IN,"<$file");
$used = '';
while (1) {
   $tc = '';
   while ($line = <IN>) {
      if ( $line =~ /^$testflagspat$/ ) {
         $dlevel_O = $1;
         $appl_O   = $2;
         $dlevel_E = $3;
         $appl_E   = $4;
         if ($6 ne $confclass) {
            $confclass = $6;
         };
         next;
      } elsif ( $line =~ /test-flags/ ) {
         log_err ("test-flag setting invalid");
         next;
      };

      if ( $line =~ /^#ifdef ([OE])(\d*)/ ) {
         $tc = "$1$2";
         $ok = ($1 eq 'O');
         if ( $tc eq '' or $tc eq 'O1' ) {
            next;
         } else {
            $line = <IN>;
            if ($line =~ /^\s*\/\/\s+test case continuation/) {
               next
            } else {
               if ($used =~ / $tc /) {
                  log_err ("test-case $tc occurs twice");
               } else {
                  $used = "$used $tc ";
                  if ( $line =~ /^\s*\/\/ \[\[([^\]]+)\]\]\s*(.*)$/ ) {
                     $err = $1;
                     $word = $2;
                  } else {
                     $err = '';
                     $word = '';
                  };
                  last
               }
            }
         }
      } else {
         $tc = ''
      }
   };

   if ($tc eq '') {
      close (IN);
      if ($case eq '') {
         exit(0)
      } else {
         log_exit ("test case $case not found");
      };
   };

   if ($case eq '' or $case eq $tc) {
      if ($ok or $err ne '') {
         $msg = '';
      } else {
         $msg = ' WITHOUT AN EXPECTED RESULT';
      };
      &log_ok ("testing $tc$msg");

      system ("$CPP -I$SE_HOME/Examples -D$tc $file > $SEF");
      system ("cp $SEF se.sta.$tc");
      $cmd = "$SE_HOME/testsuite/bin/se_run_cmp $SEF $SE_HOME ";
      if ($ok) {
         $cmd = "($cmd $dlevel_O $appl_O $SE_COMP $confclass 2>&1) |";
      } else {
         $cmd = "($cmd $dlevel_E $appl_E $SE_COMP $confclass 2>&1) |";
      };
      open (SE_TC,$cmd);
      &check_run();
      @se_out = <SE_TC>;
      close (SE_TC);

      $errors = 0;
      $fatale = 0;
      $found  = 0;
      if ($err ne '' and $word ne '') {
         $pat = "\\[\\[$err\\]\\].+$word";
      } elsif ($err ne '') {
         $pat = "\\[\\[$err\\]\\]";
      } else {
         $pat = '';
      };
      foreach $line (@se_out) { &check_errors (); };
      if (( $ok     and ($errors!=0))
          or
          (not $ok) and (($errors==0) or ($err eq '') or (not $found))
          or
          ($fatale > 0)
          or
          ($verb_level==2)
         )
      {
         foreach $line (@se_out) { print "$line"; };
      };
      if ($fatale > 0) {
         log_err ("$fatale FATALE error in $tc");
      } elsif ($ok and $errors==0) {
         log_ok ("case $tc ok");
      } elsif ($ok and $errors!=0) {
         log_err ("$errors error in $tc");
      } elsif ($errors==0) {
         log_err ("NO errors found in $tc");
      } elsif ($err eq '') {
         log_err ("$errors error in $tc without [[...]]");
      } elsif (not $found) {
         log_err ("expected error not found in $tc");
      } elsif ($err ne '' and $found and $errors==1) {
         log_ok ("case $tc ok");
      } elsif ($found and $errors>1) {
         log_ok ("case $tc ok (+ more errors)")
      } else {
         log_err ("internal error. mail reinhard.budde\@ais.fhg.de");
      };
   };

   if ($case eq $tc) { exit (0); };
};

};

sub check_errors {
   if ($line =~ /C-compiler terminated/ && !($line =~ /(0)/)) { $fatale++ };
   if ($line =~ /[uU]ndeclared/)  { $fatale++; };
   if ($line =~ /InternalError/)  { $fatale++; };
   if ($pat ne '') {
      if ($line =~ $pat) {
         $found++;
      };
   };

   if ($line =~ /^Error: \[\[UnsafeConf\]\]/) { return; };

   $ecnt = 0;
   if ($line =~ /[Ee]rror/)    { $ecnt++; };
   if ($line =~ /Failure/)     { $ecnt++; };
   if ($line =~ /[iI]llegal/)  { $ecnt++; };
   if ($line =~ /[iI]nvalid/)  { $ecnt++; };
   if ($line =~ /[pP]arse/)    { $ecnt++; };
   if ($line =~ /[tT]oken/)    { $ecnt++; };
   if ($line =~ /\[\[/)        { $ecnt++; };
   if ($ecnt) { $errors ++ };

};

1
