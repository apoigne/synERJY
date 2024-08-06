# purpose:      apply test-file to se-compiler and compare io-traces
sub test {

if ($case eq '') {
   log_ok ("dynamic test file $file");
} else {
   if ($case =~ /^T\d+$/) {
      log_ok ("dynamic test file $file test case $case");
   } else {
      log_exit ("test-case must be T<number>");
   };
};

open (IN,"<$file");
$used = '';

####### process all #ifdef's: 1 ifdef == 1 se-source-file == n traces
while (1) {
   $caseNo = '';
   readloop: while ($line = <IN>) {
      if ( $line =~ /^\/\/\s*confclass\s+(\w*)/ ) {
         if ($1 ne $confclass) {
            $confclass = $1;
         };
         next;
      } elsif ( $line =~ /^#ifdef T(\d*)\s*(.*)$/ ) {
         if ($case ne '') {
            if ("T$1" ne $case) {
               next readloop; # look for 1 case only
            };
         };
         $caseNo  = $1;
         if ($2 eq '') {
            $caseId = '';
         } else {
            $caseId = " ($2)";
         };
         if ($used =~ "<$caseNo>") {
            log_exit ("T$caseNo occurs twice in $file");
         } else {
            $used = "$used <$caseNo>";
            last;
         }
      }
   };

   if ($caseNo eq '') {
      if ($case ne '') {
         log_exit ("test-case $case not found");
      } else {
         close (IN); close (LOG);
         exit(0);
      };
   };

####### ifdef found, now generate the binary
   system ("$CPP -I$SE_HOME/Examples -DT$caseNo -DC$caseNo $file > $SEF");
   system ("cp $SEF se.dyn.T$caseNo");
   $cmd = "$SE_HOME/testsuite/bin/se_run_cmp";
   $cmd = "$cmd $SEF $SE_HOME None true $SE_COMP $confclass |";
   open (SE_TC,$cmd);
   &check_run;
   @se_out = <SE_TC>;
   close (SE_TC);
   $errors = 0;
   foreach $line (@se_out) { &check_errors () };
   if ($errors > 0 or $verb_level==2) {
      foreach $line (@se_out) { print $line }
   };
   if ($errors > 0) {
      if ($case eq '') {
         log_err ("T$caseNo$caseId: $errors error(s) when generating binary");
         next ####### continue with next test-case (i.e. next "T<n>")
      } else {
         log_exit ("T$caseNo$caseId: $errors error(s) when generating binary");
      };
   } else {
      log_ok  ("T$caseNo$caseId: binary ok");
   };

####### process the sequence of trace-definitions
   $process_trace = 1;
   while ($process_trace && ($line = <IN>)) {
     if ( $line =~ /^\s*\/\/\s+trace\s+(.*)$/ ) {
        $process_trace = 0;  ## stop:     start of first trace found
        $trace_name    = $1
     } elsif ( $line =~ /^\s*\/\/\s*$/ ) {
                             ## skip:     empty comment lines
     } else {
        log_exit ("T$caseNo$caseId: invalid trace definition (start)")
     }
   };

   $trace_cnt     = 1;
   $process_trace = 1;
   @instants      = ();
   while ($process_trace && ($line = <IN>)) {
      if ( $line =~ /^\s*\/\/\s*$/) {
                             ## skip:     empty comment lines
      } elsif ( $line =~ /^\/\/\s+(.*->.*)$/ ) {
         push(@instants,$1); ## continue: store next instant of trace
      } elsif ( $line =~ /^\/\/\s+trace\s+(.*)$/ ) {
         $next_trace_name = $1;
         &check_trace ();    ## check:    check trace, then more traces
         $trace_name = $next_trace_name;
         @instants = ();
      } elsif ( !($line =~ /^\s*\/\//) ) {
         &check_trace ();
         $process_trace = 0; ## stop:     check trace, no more traces
      } else {
         log_exit ("T$caseNo$caseId: invalid trace definition (rem)");
      }
   };

   if ($case ne '') { # process one T<num> case only
      close (IN);
      exit(0);
   };

};

};

1
