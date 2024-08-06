sub regression {

my ($p) = @_;
$p =~ s/^-//;
$p =~ s/_/-/g;

print "regression <verbose> <what>\n";
print "  <verbose> == [-S | -s | -v]  default: -s\n";
print "  <what>    == [-all|-sta-oo|-dyn-oo|-oo|-static\n";
print "               |-reactive|-examples|-min|-doc|-consistency\n";
print "               ]\n";

if ($verb_level == 0) {
   $verb = '-S';
} elsif ($verb_level == 1 ) {
   $verb = '-s';
} elsif ($verb_level == 2 ) {
   $verb = '-v';
};

if ($p eq "sta-oo") {
   &sta_oo;
} elsif ($p eq "dyn-oo") {
   &dyn_oo;
} elsif ($p eq "oo") {
   &sta_oo;
   &dyn_oo;
} elsif ($p eq "static") {
   &static;
} elsif ($p eq "reactive") {
   &reactive;
} elsif ($p =~ /^exam/) {
   &examples;
} elsif ($p =~ /^cons/) {
   &consistency;
} elsif ($p =~ /^doc/) {
   &doc;
} elsif ($p =~ /^misc/) {
   &misc;
} elsif ($p eq "all") {
   &sta_oo;
   &dyn_oo;
   &static;
   &reactive;
   &examples;
   &consistency;
   &doc;
   &misc;
} else {
   print "\nsee message above. Your (illegal) parameter was \"$p\"\n";
};
};

sub sta_oo {
    $dir = "$SE_TEST/oo";
    system("( cd $dir; \
              for f in s_*.se; do \
                $SE_HOME/testsuite/bin/fl $verb \$f; \
              done \
            )");
};

sub dyn_oo {
    $dir = "$SE_TEST/oo";
    system("( cd $dir; \
              for f in d_*.se; do \
                  $SE_HOME/testsuite/bin/fl $verb \$f; \
              done \
            )");
};

sub static {
    $dir = "$SE_TEST/static";
    system("( cd $dir; \
              for f in *.se; do \
                  $SE_HOME/testsuite/bin/fl $verb \$f; \
              done; \
            )");
};

sub whitebox {
    $dir = "$SE_TEST/whitebox";
    system("( cd $dir; \
              for f in *.se; do \
                  $SE_HOME/testsuite/bin/fl $verb \$f; \
              done \
            )");
};

sub reactive {
    open(FS,"ls -d $SE_TEST/reactive/* |");
    while ($dir = <FS>) {
      chop($dir);
      system("( cd $dir; \
                for f in *.se; do \
                    $SE_HOME/testsuite/bin/fl $verb \$f; \
                done \
              )");
    };
    close(FS);
};

sub examples {
    # copy graphics ...
    system("cp $SE_HOME/target/examples/Basic2/*.sec $WRK");
    system("cp $SE_HOME/target/examples/IncDec/*.sec $WRK");
    system("cp $SE_HOME/target/examples/Rctsty/*.sec $WRK");
    system("cp $SE_HOME/target/examples/Refine/*.sec $WRK");
    system("cp $SE_HOME/target/examples/TrafficLight/*.sec $WRK");
    system("( cd $SE_TEST/examples; \
              for f in d_*.se; do \
                  $SE_HOME/testsuite/bin/fl $verb \$f; \
              done; \
            )");
};

sub consistency {
    system("perl $SE_HOME/testsuite/bin/err_msg.pl");
    system("perl $SE_HOME/testsuite/bin/parser.pl");
    system("perl $SE_HOME/testsuite/bin/tempfiles.pl");
    
    &conssys ("$SE_TEST/examples");
    &conssys ("$SE_TEST/oo");
    &conssys ("$SE_TEST/static");
    &conssys ("$SE_TEST/whitebox");

    open(FS,"ls -d $SE_TEST/reactive/* |");
    while ($dir = <FS>) {
      chop($dir);
      &conssys ($dir);
    };
    close(FS);
};

sub conssys {
    my ($dir) = @_;
    system("( cd $dir; \
              for f in \*; do \
                  case \"\$f\" in \
                  s_\*.se) : ;; \
                  d_\*.se) : ;; \
                  \*     ) echo invalid file \\\"\$f\\\" in $dir ;; \
                  esac \
              done \
            )");
};

sub doc {
    system("cd $SE_HOME/doc/examples; \
            cp *.sec $WRK; \
            for f in *.se; do \
                $SE_HOME/testsuite/bin/sta_case $verb \$f O1; \
                fgrep -q \'#ifdef E1\' \$f; \
                case \"\$?\" in 0) \
                  $SE_HOME/testsuite/bin/sta_case $verb \$f E1 ;; \
                  \*) ;;\
                esac \
            done \
           ");
};

sub misc {
    system("( echo '=========================================================='; \
              date; \
              ls -l $SE_HOME/testsuite/misc/*.sh; \
              echo \"test files are from directory: $SE_HOME/testsuite/misc\"; \
              echo '=========================================================='; \
              cd $WRK; \
              for f in $SE_HOME/testsuite/misc/*.sh; do \
                  echo testing file \$f
                  sh \$f $verb; \
              done \
            ) | tee -a $WRK/TEST_SUITE_LOG");
};

1
