# this is a perl script used as the driver for all se-test perl-scripts.

(require "$ENV{HOME}/.setestrc")
   || die "unable to read $ENV{'HOME'}/.setestrc";

( -d "$SE_HOME" && -d "$SE_HOME/bin" && -d "$SE_HOME/target/unix/lib" &&
  -d "$SE_HOME/testsuite" && -d "$SE_HOME/testsuite/bin" &&
  -d "$WRK" && -d "$SE_TEST"
) || die "$ENV{'HOME'}/.setestrc: installation or test directories invalid\n";

if (system('cpp </dev/null >/dev/null 2>/dev/null') != 0) {
   die "$ENV{'HOME'}/.setestrc: cpp not found\n";
};

if ($ENV{'OSTYPE'} eq 'linux') {
   $CPP = "cpp -P -traditional";
} else {
   $CPP = "cpp -P -xassembler-with-cpp";
};
$CPP = "$CPP -I$SE_HOME/examples/book -I$SE_HOME/target/examples";

if ($SE_COMP eq '') {
   $SE_COMP = 'se';
};

$load = shift(@ARGV);
if ($load =~ /\/([^\/]+)$/ ) {
   $load = $1;
};

$verb_level = 1;
if ($ARGV[0] eq '-S') {
   $verb_level = 0; shift(@ARGV);
} elsif ($ARGV[0] eq '-s') {
   $verb_level = 1; shift(@ARGV);
} elsif ($ARGV[0] eq '-v') {
   $verb_level = 2; shift(@ARGV);
};

$msg = "cannot access scripts in \"$SE_HOME/testsuite/bin\"\n";
$dir = "$SE_HOME/testsuite/bin";

if ( $load eq 'v_dyn_case' or $load eq 'v_dyn_file' ) {
   (require "$dir/v_dynamic_test.pl" && require "$dir/v_sub_dyn.pl") || die $msg;
} elsif ( $load eq 'v_sta_case' or $load eq 'v_sta_file') {
   (require "$dir/v_static_test.pl") || die $msg;
} elsif ( $load eq 'v_se_testdriver' && $ENV{'USER'} eq 'budde' ) {
   print "Hallo Reinhard!\n"; exit (0);
} elsif ( $load eq 'v_regression' ) {
   (require "$dir/v_regression.pl") || die $msg;
   &v_regression (shift(@ARGV));
   exit(0);
} else {
   die "invalid call of test driver (was: \"$load\")";
};

$SRC = `pwd`; chop($SRC);
chdir ($WRK);
$c = "$SE_HOME/bin/se_dbg";              # info printed later by &start_logging
open (IN,"(date;ls -go $c) |");          # info printed later by &start_logging
@log = <IN>;                             # info printed later by &start_logging
close (IN);                              # info printed later by &start_logging

$file    = shift(@ARGV);
$file    = "$SRC/$file";

$case    = shift(@ARGV);

$error   = shift(@ARGV);
if ( $error eq '' ) {
} else {
   die "too many commands for test driver (was: \"$error\")";
};

&start_logging;

if ($load eq 'sta_case' && $case eq '') {
   $case = 'O1';
} elsif ($load eq 'v_dyn_case' && $case eq '') {
   log_exit ('test case name is missing');
};

if ($file eq '') {
   log_exit ('name of test file is missing');
} elsif (!($file =~ /\.se$/)) {
   $file  = "$file.se";
};
if (($load eq 'sta_file' or $load eq 'v_dyn_file') && $case ne '') {
   log_exit ('sta_file or v_dyn_file may not be used with a test case');
};

if (not (-r $file)) {
   log_exit ("test file is not accessible");
};

&test;
log_exit ('unexpectedly test driver got control - mail reinhard.budde\@gmd.de');

sub start_logging {
    open (LOG,'>>TEST_SUITE_LOG');
    print LOG "\n===========================================================\n";
    foreach $l (@log) {
      $l =~ s/^[ ]*//;
      print LOG "$l";
    };
    print LOG "test files are from directory: $SRC\n";
    # print LOG "debug level: $SE_DBGL, simulation level: $SE_SIML\n";
};

sub log_exit {
    my ($msg) = @_;
    print "$SIG $msg. File $file $FLT\n";
    print LOG "$SIG $msg $FLT\n";
    exit (1);
};

sub log_ok {
    my ($msg) = @_;
    if ($verb_level > 0) {
       print "$SIG $msg\n";
    };
    print LOG "$msg\n";
};

sub log_err {
    my ($msg) = @_;
    print "$SIG $msg. File $file $FLT\n";
    print LOG "$SIG $msg $FLT\n";
};

sub check_run {
    my ($l);
    $l = <SE_TC>;
    $l = <SE_TC>;
    if ($l =~ /execute immediate command: set debug level =/) {
       return;
    } else {
       log_exit ('se_run_* did not work - compiler not installed properly?');
    };
};
