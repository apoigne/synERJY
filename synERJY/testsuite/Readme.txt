OVERVIEW
========

To validate the sE-compiler, a test-suite is used, available at
$SE_HOME/testsuite. The directory contains the directory 'bin', in which the
(perl-) scripts are stored, which do the work. This directory should be in the
PATH of the user of the test-suite. The directory $SE_HOME/testsuite contains
further many other directories containing the test files.

In the resource file $HOME/.setestrc some variables are defined that control
the tests. The file must contail valid perl statements. It must have '1' as
last statement to indicate successfull execution. An example of .setestrc is
in the appendix of this Readme.

A test-file is responsible to check the validity of the compiler w.r.t. a
problem area. A test-file defines

  1. the common context for all test-cases, e.g. auxiliary classes, variables.
  2. test-cases.
  3. trace-definitions.

Test-cases of 2. are used to check static properties, e.g. typing of programs.
The trace-definitions of 3. are used to check dynamic properties, e.g. what
output sequence a program should produce given a sequence of inputs.
Remark: In the following <n> is a positive integer w.o. leading zeroes, e.g. 12.

All test-scripts accepts as first parameter a verbosity-level:
-  -S  very silent, report only errors
-  -s  silent,      report ok and errors
-  -v  verbose,     print ok, errors, compiler output, ...

1. CHECKING STATIC SEMANTICS
============================

A test-file to check static semantics contains a sequence of test-cases.

Test-cases have names:

  - positive ('ok') test-case are named O<n>. These test-cases subsumes all
    those cases, which should pass the compiler without error. The ok
    test-cases denote legal, sometimes obvious, sometimes bizarre, but allowed
    usage of the language.
  - negative ('error') test-case are named E<n>. The error test-cases denote
    a specific illegal usage of concepts of the language. The compiler should
    detect that and issue the error-message declared in the test-case.

A test "name" was sucessful, if the test-file "name.se" was executed and
- the ok test-cases produce no errors,
- all error test-cases produced the error specified in the test-case.

1.1 SCRIPTS

  Using the static part of the test-suite is supported by two scripts:
  - sta_file <file>
             executes all test-cases from one test-file
  - sta_case <file> <test-case name, e.g. O4, E6 ...>
             executes one test-case. If test-case name is missing, uses O1.

1.2 HOW THE SCRIPTS WORK

- A test-case, for instance "E5", is extracted from test-file "test" by
  running cpp: "cpp -DE5 test". Thus test-files must be cpp-able.
- The cpp-run produces a se-program stored in a temporary file. The temporary
  file is compiled by the sE-compiler and the output processed.

1.3 BUILDING THE ERROR TEST CASES

An test-case, for instance "E5", is defined in a test-file by surrounding
the line(s) which are specific for this test case by
#ifdef E5
   // [[ErrorCode]]
   << error line(s) >>
#endif

Even if no "#ifdef O1" is found in the test-file, at least test O1 is assumed
to exist in any test-file.

Sometimes a test-case needs lines at many places in the test-file.
In this case all #ifdef's belonging to a single test-case but one must
contain a special comment "// test case continuation". Otherwise the scripts
report that a test-case name is used twice (considered to be an error).
For instance, if test-case "E5" from above needs further lines to be fully
defined, thus can be done by writing
#ifdef E5
   // test case continuation
   << further error line(s) >>
#endif

1.4 EXAMPLE

see file "example_sta.se" in this directory
The file contains 3 test-cases: O1, E1 and E2. The errors expected are:
- for O1: no error. This test-case is implicitly defined.
- for E1: Subtyping, because Boolean is no subtype of Integer
- for E2: UnknownId, because identifier bi is not declared in class Tc

Execution of the commands:
./bin/sta_file example_sta    or   ./bin/sta_file example_sta.se
produces the output:

========== static test file /home/budde/sE20/testsuite/example_sta.se
========== testing O1
========== case O1 ok
========== testing E1
========== case E1 ok
========== testing E2
========== case E2 ok

2. CHECKING DYNAMIC SEMANTICS
=============================

The test-file to check dynamic semantics contains many tests. The name
of a test is T<n>.
Each test T<n> refers to one se-program, which is the system under test.
Each test T<n> defines a sequence of traces.
Each trace defines a sequence of instants.
Each instants defines the input signals, which are supplied to the system
under test, and the output signals, which are expected as response.

A test "name" was sucessful, if
- all tests T<n> from test-file "name.se" generate binaries without error and
- all trace definitions of all tests are run without errors.

2.1 SCRIPTS

  Using the dynamic part of the test-suite is supported by scripts,
  script "dyn_file" is used in most cases:
  - dyn_file <file>
              executes all tests with all traces as defined in the test-file.
  - dyn_case <file> <test-case name, e.g. T1>
              executes all traces of the specified test.

2.2 HOW THE SCRIPTS WORK

- A test, for instance "T5", is extracted from test-file "test" by
  running cpp: "cpp -DT5 test". Thus test-files must be cpp-able.
- The cpp-run produces a se-program stored in a temporary file. The temporary
  file is compiled by the sE-compiler and the binary "se.a.out" is generated.
- For each test a sequence of traces is defined as comments with suitable
  syntax.
- A trace is read. The input signals for each of its instants are extracted
  and written into a file. The binary "se.a.out" was generated in simulation
  mode and reads this file, and generate a file with the output signals for
  each instant. This is compared with the expected signals from the
  trace-definition.

- The tests to be considered are
  - extracted automaticaly from the test-file (script "dyn_file")
  - defined by the user (script "dyn_case")

2.3 BUILDING THE TEST FILE
  
An test, for instance "T5", is defined in a test-file by using the line(s)
#ifdef T5
   << trace 1 >>
   << trace ... >>
   << trace n >>
   << further lines not part of the traces, may be empty >>
#endif

The file generated by processing the test-file with "cpp -DT5 ..." must
contain a valid sE-program. The traces are expected immediately after the
"#ifdef".

The traces are special comments, starting with "// " in column 1 of the line.
Comment lines "//" containing only white space are skipped.

- a line "// trace" with an optional remark starts a trace.
- a line which contains "//" at he beginning and later "->" defines an
  instant of a trace. Input signals are written before "->", output signals
  afterwards.
- Each signal list is either empty (written as white space), or contains one
  or more signals. Signals are separated by white-space.
- Each supplied or expected signal is written "<signal_name>", if the
  signal is unvalued, or "<signal_name>(<signal-value>)", if the signal is
  valued. At the moment only single-valued signals are supported.

2.4 EXAMPLE

see file example_dyn.se in this directory
The file contains 3 traces. The last contains an error (expected signal does
not show up).

Execution of the commands:
./bin/dyn_file example_dyn    or   ./bin/dyn_file example_dyn.se
produces the output:

========== dynamic test file /home/budde/sE20/testsuite/example_dyn.se
========== T1 (NO-ERROR-CASE 0): binary ok
========== T1 (NO-ERROR-CASE 0): trace 1 ok
========== T2 (ERRORS-IN-3-TRACE): binary ok
========== T2 (ERRORS-IN-3-TRACE): trace 1 ok
========== T2 (ERRORS-IN-3-TRACE): trace 2 ok
========== T2 (ERRORS-IN-3-TRACE): 1 error(s) in trace 3. File /home/budde/sE20/testsuite/example_dyn.se THIS IS A DETECTED FAULT
     ===== error in 4. instant
           expected: osig
           found:    
========== T2 (ERRORS-IN-3-TRACE): trace 4 ok

3. STATUS OF THE TEST-SUITE
===========================

the script "./bin/regression <what>" will check, whether the compiler found
in the test-suite-resource file "$HOME/.setestrc" is valid.
Run "./bin/regression" to see which <what> is available

4. APPENDIX: file .setestrc

##################################
####### environment data
##################################

$SE_HOME = "/home/budde/sE20";               # the compiler installation dir
$SE_TEST = "/home/budde/sE20/testsuite";     # the test directory for regression
#$SE_COMP = "se_opt";                         # byte-code = se, native = se_opt

$SIG = '==========';                # BIG attention
$Sig = '     =====';                # some attention
$sig = '          ';                # little attention

$PRINT_DIR = 0;                     # don't print dir name

$FLT = 'THIS IS A DETECTED FAULT';  # errors indicator (used as suffix of msg)

$WRK = "$ENV{'HOME'}/tmp/se_test";  # the dir to store generated files

$SEI = 'se.generated_input';        # auxiliary file to control the compiler
$SEO = 'se.generated_output';       # auxiliary file for the compiler output
$SEF = 'se.test_file.se';           # auxiliary file for the sE-source

1                                   # loading WAS SUCCESSFUL
