#!/bin/sh
PROG_FILE=tests_and_demos/t$1.scm

if [ -e $PROG_FILE ] ; then
  echo "/// source code"
  echo "----------------"
  cat $PROG_FILE
  echo "----------------"
  echo "/// Gauche"
  gosh $PROG_FILE
  echo "/// Scheme on PIC"
  /usr/local/bin/gosh t2.scm $PROG_FILE
  pk2 -off
  pk2 -write _ipstest.hex
  pk2 -on
  sleep 10
  pk2 -off
else
  ls tests_and_demos/t*.scm | sed 's|tests_and_demos/t||g; s|\.scm||g'
fi

