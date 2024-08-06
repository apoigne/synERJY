#########################################
####### subroutines used in dyn_*
#########################################

#########################################
####### check ONE trace-definition ######
#########################################

sub check_trace {

system ("rm -f $SEI $SEO");

####### generate the INPUT file for the simulation-binary
open (SE_TC,">$SEI");
foreach $instant (@instants) {
   $instant =~ /^(.*)->/;
   @sigs = &split ($1);
   foreach $sig (@sigs) {
      $sig = &mk_sig ($sig);
      print SE_TC "$sig\n";
   };
   print SE_TC "++i\n";
};
print SE_TC "++abort\n";
close (SE_TC);

####### generate the OUTPUT file by calling the simulation-binary
system ("./se.a.out <$SEI >$SEO");

open (SE_TC,"<$SEO");
@sigs    = ();
$errors  = 0;
$proceed = 1;
$instcnt = 1;

####### skip the initial data from the OUTPUT file
%nooutput  = ();

while ($proceed && ($line = <SE_TC>)) {
   if ($line =~ /^\$local\S*\s+(\S*)/) {
      $noout = $1;
      $nooutput{$noout} = 1;
   } elsif ($line =~ /^\$sensor\S*\s+(\S*)/) {
      $noout = $1;
      $nooutput{$noout} = 1;
   } elsif ( $line =~ /^\$configuration/ ) {
      $proceed = 0;
   };
};
if ($proceed > 0) {
   log_exit ("T$caseNo$caseId: invalid initial data of system under test");
};

####### read the sequence of instants
$read = 1;
$error = "";
while ($read && ($line = <SE_TC>)) {
   if ( $line =~ /^at:/ or $line =~ /^\/\//) {
   } elsif ( $line =~ /^--i/ ) {
      ####### check one instant
      $instant = shift(@instants);
      $instant =~ /->\s*(.+)$/;
      @sige = &split ($1);
      @sigs = sort(@sigs); @svs = @sigs;
      @sige = sort(@sige); @sve = @sige;
      $proceed = 1;
      while ($proceed) {
        $s1 = shift(@sigs); $s2 = shift(@sige);
        if ($s1 eq "" && $s2 eq "") {
           $proceed = 0;
        } elsif ($s1 eq "" or $s2 eq "" or $s1 ne $s2) {
           $l1 = "$Sig error in $instcnt. instant\n";
           $l2 = "$sig expected: @sve\n$sig found:    @svs\n";  
           $error = "$error$l1$l2";
           $errors++;
           $proceed = 0;
        }
      };
      @sigs = ();
      $instcnt++;
   } elsif ( $line =~ /^\$abort/ ) {
      ####### all instants checked
      $read = 0;
   } elsif ( $line =~ /^-(\S+)\s+\(\)$/ ) {
      ####### add the pure output signal to the instant-list
      if ($nooutput{$1}) {
         # ignore all non output signals
      } else {
         push(@sigs,$1);
      };
   } elsif ( $line =~ /^-(\S+)\s+\((.*)\)$/ ) {
      ####### add the valued output signal to the instant-list
      if ($nooutput{$1}) {
         # ignore all non output signals
      } else {
         push(@sigs,"$1($2)");
      };
   } else {
      ####### invalid data from simulation-binary
      $errors++;
      log_exit ("T$caseNo$caseId: invalid instant data");
   };
};

####### now all instants are checked: did an error show up?
if ($trace_name ne "") {
   $trace_name = " (trace name: $trace_name)";
}
if ( @sigs == () && $errors == 0 && @instants == () ) {
   log_ok ("T$caseNo$caseId: trace $trace_cnt ok");
} elsif ( $errors > 0 ) {
   log_err ("T$caseNo$caseId: $errors error(s) in trace $trace_cnt");
   print $error
} elsif ( !(@instants == ()) ) {
   log_err ("T$caseNo$caseId: se.a.out aborted(1) in trace $trace_cnt")
} else {
   log_err ("T$caseNo$caseId: se.a.out aborted(2) in trace $trace_cnt")
};
$trace_cnt++;
};

sub output_trace {

  system ("rm -f $SETR");
  
  ####### generate the INPUT file for the simulation-binary
  open (SE_TC,">$SETR");
  foreach $instant (@instants) {
    $instant =~ /^(.*)->/;
    @sigs = ();
    @sigs = &split ($1);
    foreach $sig (@sigs) {
        $sig = &mk_sig_trace ($sig);
        print SE_TC "$sig ";
    };
    
    print SE_TC "-> ";
    $instant =~ /->\s*(.*)$/;
    @sigs = ();
    @sigs = &split ($1);
    foreach $sig (@sigs) {
        $sig = &mk_sig_trace ($sig);
        print SE_TC "$sig ";
    };
    
    print SE_TC ";\n";
    
  };
  close (SE_TC);
  
};

sub split {
   my ($str) = @_;
   my (@sl);
   if ( $str =~ /^\s*$/ ) {
      return ();
   } else {
      $str =~ s/' '/___/g;
      @sl = split(/\s+/,$str);
      foreach $str (@sl) {
        $str =~ s/___/' '/g;
      };
      return @sl;
   };
};

sub mk_sig {
   my ($str) = @_;
   if ( $str =~ /^(\S+)\((.+)\)$/ ) {
      $str = $2;
      $str = "+$1 ($str)";
   } else {
      $str = "+$str ()"; };
   return $str;
};

sub mk_sig_trace {
   my ($str) = @_;
   if ( $str =~ /^(\S+)\((.+)\)$/ ) {
      $str = $2;
      $str = "$1 ($str)";
   } else {
      $str = "$str ()"; };
   return $str;
};

sub check_errors {
   if ($line =~ /^Error: \[\[UnsafeConf\]\]/) { return; };

   $ecnt = 0;
   if ($line =~ /[uU]ndeclared/) { $ecnt++ };
   if ($line =~ /[Ee]rror/)      { $ecnt++ };
   if ($line =~ /Failure/)       { $ecnt++ };
   if ($line =~ /[iI]llegal/)    { $ecnt++ };
   if ($line =~ /[iI]nvalid/)    { $ecnt++ };
   if ($line =~ /[pP]arse/)      { $ecnt++ };
   if ($line =~ /[tT]oken/)      { $ecnt++ };
   if ($line =~ /exception/)     { $ecnt++ };
   if ($line =~ /C-compiler terminated/ && !($line =~ /(0)/)) { $ecnt++ };

   if ($ecnt) { $errors ++ };

};

1
