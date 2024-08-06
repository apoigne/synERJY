#!/opt/gnu/bin/perl
# this is a perl script used to create se-examples

( -d "$ENV{'SE_HOME'}"
) || log_err ("sE home directory invalid: \$SE_HOME=$ENV{'SE_HOME'}");

$verb_level = 1;
if ($ARGV[0] eq '-S') {
   $verb_level = 0; shift(@ARGV);
} elsif ($ARGV[0] eq '-s') {
   $verb_level = 1; shift(@ARGV);
} elsif ($ARGV[0] eq '-v') {
   $verb_level = 2; shift(@ARGV);
};

$ERR     = '>>>> ERROR >>>>';

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

open (IN,"<$file");
while ($line = <IN>) {
   if ( $line =~ /^\/\/\s+tex_start/ ) {
      if ( $line =~ /^\/\/\s+tex_start\s+(\S+)$/ ) {
         $name = $1;
      } elsif ( $line =~ /^\/\/\s+tex_start\s+$/ ) {
         if ( $file =~ /^(.*).tex$/ ) {
            $name = $1;
         } else {
            log_err ("form file $file .tex could not be removed");
         };
      } else {
         log_err ("tex_start line has invalid syntax: $line");
      };
      if ( $name =~ /^(.*).se$/ ) {
         $name = $1;
      };
      if ($example eq '' or $example eq $name) {
         $name = "../Input/se.$name.tex";
         log_ok ("processing example into out file $name");
         prc_example ();
      };
   };
};
close (IN);
log_ok ("file $file processed");
exit(0);

sub prc_example {
    # removed after Axel complained about too defensive programming style
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
       $line =~ s/\$/\\\$/g;
       $line =~ s/\&/\\\&/g;
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
