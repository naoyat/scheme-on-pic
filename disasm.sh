#!/bin/sh

TMPFILE=/tmp/disasm.$$

echo "(disasm (lambda ()" > $TMPFILE
cat $1 >> $TMPFILE
echo "))" >> $TMPFILE

gosh $TMPFILE

rm -f $TMPFILE

