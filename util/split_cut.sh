#!/bin/bash -
# checking if bsplit and cutter are installed
command -v bsplit >/dev/null 2>&1 || { echo >&2 "I require bsplit but it's not installed. Aborting."; exit 1; }
command -v cutter >/dev/null 2>&1 || { echo >&2 "I require cutter but it's not installed. Aborting."; exit 1; }
function print_usage {
  echo "Usage: `basename $0` -l #number_of_splits_levels [-bsplit] [-cutter [check_only]]"
}
if [ $# -eq 0 ] ; then
  echo "The maximum number of splits level must be passed as argument"
  print_usage
  exit 1
fi
#parsing command line
chk=="not_check_only"
while [ "$1" != "" ]; do
  case $1 in
    -l)
      shift; Nl=$1; shift
      ;;
    -bsplit)
      Bsp="yes"; shift
      ;;
    -cutter)
      Cut="yes"; shift; chk=$1; shift
      ;;
    \?)
      print_usage
      exit 1
      ;;
  esac
done
if [ "$chk" == "check_only" ] ; then
  cutchk=1
else
  cutchk=0
fi
if [ "$Bsp" == "yes" ] ; then
  rm -f bsplit*.log
  for l in $(seq 1 $Nl); do
    echo "Running bsplit over level $l"
    lz=`printf "%4.4d" $l`
    ln -fs bsplit$lz.par bsplit.par
    bsplit >> bsplit$lz.log 2>&1
  done
fi
if [ "$Cut" == "yes" ] ; then
  echo "Running cutter"
  lz=`printf "%4.4d" $Nl`
  cc=`cat bsplit$lz.par | grep SPLIT | tail -n 1 | awk '{print $1}' | sed "s/'//g"`
  cc="'"$cc"SPLIT'"
  echo $cutchk > cutter.par
  echo $cc >> cutter.par
  echo no_file_sol >> cutter.par
  echo $cc >> cutter.par
  echo 1 >> cutter.par
  echo 1 >> cutter.par
  cutter < cutter.par > cutter.log 2>&1
fi
