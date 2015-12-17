#!/bin/bash -
# checking if bsplit and cutter are installed
command -v bsplit >/dev/null 2>&1 || { echo >&2 "I require bsplit but it's not installed. Aborting."; exit 1; }
command -v cutter >/dev/null 2>&1 || { echo >&2 "I require cutter but it's not installed. Aborting."; exit 1; }
function print_usage {
  echo "Usage: `basename $0` -l #number_of_splits_levels [-bsplit] [-cutter [do/check_only]] [-extract [link/copy/move]]"
}
if [ $# -eq 0 ] ; then
  echo "The maximum number of splits level must be passed as argument"
  print_usage
  exit 1
fi
#parsing command line
chk=="not_check_only"
Bsp="no"
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
    -extract)
      Extract="yes"; shift; ext=$1; shift
      ;;
    *)
      print_usage
      exit 1
      ;;
  esac
done
if [ "$Cut" == "yes" ] ; then
  case $chk in
    do)
      cutchk=T
     ;;
    check_only)
      cutchk=F
     ;;
    *)
      print_usage
      exit 1
     ;;
  esac
fi
if [ "$Extract" == "yes" ] ; then
  case $ext in
    link)
      Ext="ln -s "
     ;;
    copy)
      Ext="cp "
     ;;
    move)
      Ext="mv "
     ;;
    *)
      print_usage
      exit 1
     ;;
  esac
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
  if [ -f bsplit$lz.par ]; then
    cc=`cat bsplit$lz.par | grep SPLIT | tail -n 1 | awk '{print $1}' | sed "s/'//g"`
    if [ "$cc" == "" ] ; then
      cc='ccSPLIT'
    fi
  else
    cc='cc'
  fi
  echo $cutchk > cutter.par
  echo $cc >> cutter.par
  cutter > cutter.log 2>&1
fi
if [ "$Extract" == "yes" ] ; then
  if [ "$Cut" == "yes" ] ; then
    mkdir -p output
    cd output
    $Ext ../proc.input .
    $Ext ../balanced-blk_groups.ini blk_groups.ini
    if [ "$Bsp" == "yes" ] ; then
      $Ext ../ccSPLIT*p??? .
      perl-rename "s/SPLIT//g" ccSPLIT*
      rm -f ../ccSPLIT*
    else
      $Ext ../cc*p??? .
    fi
    cd -
  fi
fi
exit 0
