#!/bin/sh

VERB=$1

testit () {
  CASE="$1"
  echo "testing throw.se case $CASE"
  rm -f se.a.out se.Rc.[ch]
  RC='error'
  ( cpp -P -D$CASE $SE_HOME/testsuite/misc/throw.se > se.throw_$CASE.se
    $SE_HOME/bin/se -f "%target Simulation;set workspace = <synERJY>/testsuite/misc;set project path = <synERJY>/testsuite/misc;load file=<synERJY>/testsuite/misc/se.throw_$CASE.se;make C-code;quit;"
    gcc se.Rc.c -Dse_sim -Dse__unix -o se.a.out \
        throw.o $SE_HOME/target/unix/lib/libse_rt.a \
        -I $SE_HOME/include -I $SE_HOME/target/unix/include \
    && RC=`./se.a.out`
    echo 'se.a.out returned:'
    echo "$RC"
  ) >se.tmp 2>&1

  RC=`./se.a.out`;

  if [ -x se.a.out ]; then
     case "$VERB:$RC" in
      -v:*) cat se.tmp ;;
      *:${CASE}\ ok) :          ;;
      * )   cat se.tmp ;;
     esac
   else
     RC="$CASE error"
     echo generation of se.a.out failed
     cat se.tmp
   fi

  case "$RC" in
     ${CASE}\ ok) echo "test of misc/throw $CASE standalone test ok"   ;;
     *) echo "========== test of misc/throw $CASE standalone test failed"
        echo "========== THIS IS A DETECTED FAULT" ;;
  esac
}

gcc -c $SE_HOME/testsuite/misc/throw.c -Dse_sim -Dse__unix \
    -I $SE_HOME/include -I $SE_HOME/target/unix/include

testit "T1"
testit "T2"
testit "T3"
testit "T4"
testit "T5"
testit "T6"
testit "T7"
testit "T8"
testit "T9"
