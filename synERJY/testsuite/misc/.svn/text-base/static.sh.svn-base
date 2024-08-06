#!/bin/sh

VERB=$1

( rm -f se.Rc.[ch] static.o se.a.out
  gcc -c $SE_HOME/testsuite/misc/static.c -Dse__$OSTYPE \
      -I $SE_HOME/include -I $SE_HOME/target/unix/include
  $SE_HOME/bin/se -b "%load file=$SE_HOME/testsuite/misc/static.se;\
      target Simulation;make C-code;quit;"
  gcc se.Rc.c -Dse__$OSTYPE -o se.a.out \
      static.o $SE_HOME/target/unix/lib/libsert.a \
      -I $SE_HOME/include -I $SE_HOME/target/unix/include
) >se.tmp 2>&1
RC=`se.a.out`

case "$RC:$VERB" in
 ok:*) cat se.tmp ;;
 *:-v) cat se.tmp ;;
 * )   : ;;
esac

case "$RC" in
   ok) echo "test of misc/static standalone test ok"   ;;
   * ) echo "========== test of misc/static standalone test failed"
       echo "========== THIS IS A DETECTED FAULT" ;;
esac
