#!/bin/bash
# shell script to translate a Prolog program with the
# fail-sensitive functional transformation

PMOD=`basename $1 .pl`
CMOD=${PMOD^}

# check existence of pl2curry
PLCURRY=`command -v pl2curry`
if [ -z "$PLCURRY" ] ; then
  echo "Required executable 'pl2curry' not found!"
  echo "Install it by: 'cypm update && cypm install prolog2curry'"
  exit 1
fi

# check existence of curry-calltypes
CURRYCALLTYPES=`command -v curry-calltypes`
if [ -z "$CURRYCALLTYPES" ] ; then
  echo "Required executable 'curry-calltypes' not found!"
  echo "Install it by: 'cypm update && cypm install verify-non-fail'"
  exit 1
fi

set -e

echo ">>> Translate '$PMOD' with demand functional transformation:"
$PLCURRY $PMOD

echo ">>> Analyze possible failing operations:"
$CURRYCALLTYPES --storefuncs --nosmt $CMOD

echo ">>> Translate '$PMOD' with fail-sensitive functional transformation:"
$PLCURRY --failfuncs=$CMOD.FAIL $PMOD

/bin/rm -f $CMOD.FAIL $CMOD.NONFAIL
