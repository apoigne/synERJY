#!/bin/sh

VERB=$1

( rm -f se.Rc.[ch] callback.o se.a.out
  gcc -c $SE_HOME/testsuite/misc/callback.c -Dse__$OSTYPE
  $SE_HOME/bin/se -b "%load file=$SE_HOME/util/Stderr.se;\
      load file=$SE_HOME/testsuite/misc/callback.se;\
      target Simulation;make C-code;quit;"
  gcc se.Rc.c -Dse__$OSTYPE -o se.a.out \
      callback.o $SE_HOME/target/linux/lib/libsert.a \
      -I $SE_HOME/include -I $SE_HOME/target/linux/include
) >se.tmp 2>&1
se.a.out 2>se.out
diff $SE_HOME/testsuite/misc/callback.out se.out

RC=$?

case "$VERB" in
 -v) cat se.tmp ;;
 * )   : ;;
esac

case "$RC" in
   0) echo "test of misc/callback standalone test ok"   ;;
   *) echo "========== test of misc/callback standalone test failed"
      echo "========== THIS IS A DETECTED FAULT" ;;
esac
