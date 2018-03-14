#!/bin/sh
#
# wrapper script for MkFrags.mkMakefile function
#

PROG=mkmk

if [ $# != 1 ] ; then
  echo "usage: $PROG.sh <dir>"
  exit 1
fi

DIR=$1

SRC=/Users/chariseechiw/diderot/femprime/src/compiler/gen/fragments/sources.cm

if test "smlnj" = "smlnj" ; then
/usr/local/smlnj/bin/sml $SRC 2> /dev/null 1>/dev/null <<XXXX
MkFrags.mkMakefile "$DIR";
XXXX
exit $?
elif test "smlnj" = "mlton" ; then
  HERE=$(pwd)
  cd /Users/chariseechiw/diderot/femprime/src/compiler/gen/fragments
   -output $PROG sources.mlb || exit $1
  cd $HERE
  /Users/chariseechiw/diderot/femprime/src/compiler/gen/fragments/$PROG $DIR || exit $1
  rm -f /Users/chariseechiw/diderot/femprime/src/compiler/gen/fragments/$PROG
  exit 0
else
  echo "unknown SML implementation"
  exit 1
fi
