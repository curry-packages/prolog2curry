#!/bin/bash
# shell script to translate a Prolog program with the
# fail-sensitive functional transformation

showhelp () {
  echo "Usage:"
  echo ""
  echo "    pl2curry-failsensitive [OPTIONS] PROLOGFILE"
  echo ""
  echo "with options:"
  echo ""
  echo "-h|--help|-? : show this message and quit"
  echo "-q|--quiet   : quiet mode, show only generated Curry program"
  echo "-v1          : show status messages (default)"
  echo "-v2          : show intermediate results (same as -v)"
  echo "-v3          : show more details"
  exit
}

HELP=no
VERBOSE=
for arg do
  case $arg in
    --quiet | -q         ) VERBOSE=-q   ;;
    -v | -v1 | -v2 | -v3 ) VERBOSE=$arg ;;
    --help  | -h | -\?   ) HELP=yes     ;;
    -*                   ) echo "Illegal argument: $arg" ; exit 1 ;;
    *                    ) set -- "$@" "$arg" ;;
  esac
  shift
done

if [ $HELP = yes ] ; then
  showhelp
fi

if [ $# -eq 0 ] ; then
  echo "Name of Prolog file missing!"
  echo "(use --help for details)"
  exit 1
fi

if [ $# -gt 1 ] ; then
  echo "Too many file arguments!"
  exit 1
fi

echoNQ () {
  if [ "$VERBOSE" != "-q" ] ; then
    echo $1
  fi
}

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

echoNQ ">>> Translate '$PMOD' with demand functional transformation:"
$PLCURRY $VERBOSE $PMOD

echoNQ ">>> Analyze possible failing operations:"
$CURRYCALLTYPES $VERBOSE --nosmt --storefuncs $CMOD

echoNQ ">>> Translate '$PMOD' with fail-sensitive functional transformation:"
$PLCURRY $VERBOSE --failfuncs=$CMOD.FAIL $PMOD

/bin/rm -f $CMOD.FAIL $CMOD.NONFAIL
