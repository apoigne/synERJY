#!/usr/bin/perl
# this is a perl script used to create se-examples
# usage is: perl mk_exmpl.pl [{-S | -s | -v}] <file> [<example>]
#       or: perl $SE_HOME/doc/bin/mk_exmpl.pl -all
# e.g.: cd $SE_HOME/doc/examples;
#       perl $SE_HOME/doc/bin/mk_exmpl.pl chpt_2_belt.se

$SE_HOME = "$ENV{'SE_HOME'}";
$SE_DOC  = "$SE_HOME/doc";
$SE_EXMP = "$SE_DOC/examples";
$SE_PERL = "$SE_DOC/bin/mk_exmpl.pl";

( -d $SE_HOME
) || log_err ("sE home directory invalid: \$SE_HOME=$SE_HOME");
( -d $SE_DOC
) || log_err ("sE document directory invalid: $SE_DOC");

$verb_level = 1;
$example = '';
if ($ARGV[0] eq '-S') {
   $verb_level = 0; shift(@ARGV);
} elsif ($ARGV[0] eq '-s') {
   $verb_level = 1; shift(@ARGV);
} elsif ($ARGV[0] eq '-v') {
   $verb_level = 2; shift(@ARGV);
} elsif ($ARGV[0] eq '-all') {
   $example = 'all'; shift(@ARGV);
};

$ERR     = '>>>> ERROR >>>>';

if ($example eq 'all') {
   open(IN,"cd $SE_EXMP;ls *.se |");
   @files = <IN>;
   close(IN);
   foreach $f (@files) {
     chop($f);
   };
   $len = @files;
   print "example files to consider: $len\n";
   foreach $f (@files) {
     system("perl $SE_PERL $f");
   };
   exit(0);
};

$file    = shift(@ARGV);
$example = shift(@ARGV);
$error   = shift(@ARGV);

if ($file eq '') {
   log_err ("file name must be given as anonymous parameter");
} elsif ( $error ne '' ) {
   log_err ("too many parameters (the first of them was: \"$error\")");
};

if ($example eq '') {
   $msg = "example generation from file $file";
} else {
   $msg = "example generation from file $file, example $example";
};
log_ok ($msg);

open (IN,"<$SE_EXMP/$file");
while ($line = <IN>) {
   chop($line);
   if ( $line =~ /^\/\/\s+tex_start/ ) {
      if ( $line =~ /^\/\/\s+tex_start\s+(\S+)$/ ) {
         $name = $1;
      } elsif ( $line =~ /^\/\/\s+tex_start\s+$/ ) {
         if ( $file =~ /^(.*).se$/ ) {
            $name = $1;
         } else {
            log_err ("file name $file has an invalid name");
         };
      } else {
         log_err ("tex_start line has invalid syntax: $line");
      };
      if ( $name =~ /^(.*).se$/ ) {
         $name = $1;
      };
      if ($example eq '' or $example eq $name) {
         $name = "$SE_DOC/tex_input/se.$name.tex";
         log_ok ("tex into $name");
         prc_example ();
      };
   };
};
close (IN);
log_ok ("file $file processed");
exit(0);

sub prc_example {
    # removed after Axel complained about too defensive programming style:
    # if (-f "$name") {
    #    log_err ("tex outfile does exist, will not be overwritten");
    # };
    open (OUT,">$name");
    print OUT "\\BEP\n";
    $eof = 1;
    while ($line = <IN>) {
       if ( $line =~ /^\/\/\s+tex_end/ ) {
          $eof = 0;
          last;
       };
       $line =~ s/\{/\\\{/g;
       $line =~ s/\}/\\\}/g;
       $line =~ s/\_/\\\_/g;
       $line =~ s/\^/\\\^/g;
       $line =~ s/\&/\\\&/g;
       $line =~ s/\$/\\\$/g;
       if ($line =~ s/\/\//\/\/\\textit{/) {
          $line =~ s/$/\}/;
       };
       print OUT "$line";
    };
    if ($eof) {
       log_err ("eof encountered during processing example $name");
    } else {
       print OUT "\\EEP\n";
       close (OUT);
       return;
    };
};

sub log_ok {
    my ($msg) = @_;
    if ($verb_level > 0) {
       print "$msg\n";
    };
};

sub log_err {
    my ($msg) = @_;
    print "$ERR $msg\n";
    exit(1);
};
