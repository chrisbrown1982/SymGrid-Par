#!/bin/bash
# Time-stamp: <Mon Jun 28 2010 22:49:00 Stardate: Stardate: [-28]3294.33 hwloidl>
# $Id$
# -----------------------------------------------------------------------------
#
# Usage: sgp_admin <cmd> [options]
#
# Administrative commands for the SymGrid-Par installation.
#
# Commands:
#  init ... run init script of the installation
#  launch ... launch servers specified in a <file> argument (default: $SGPRC)
#  start ... start coordination server
#  kill ... kill running servers etc
#
# Options:
#  -h ... help message
#  -v ... be verbose
#  -V ... show version
#
# -----------------------------------------------------------------------------

function pvm_start {
  echo "config" | pvm
}

function pvm_stop {
  echo "halt" | pvm
}

help=0
verbose=0
show_version=0
# default architecture
hwos=""

cs_port=12321 

getopts "hvVa:" nam
while [ "$nam" != "?" ] ; do
  case $nam in
   h) help=1;;
   v) verbose=1;;
   V) show_version=1;;
   a) hwos="$OPTARG";;
  esac 
  getopts "hvVa:" nam
done

shift $[ $OPTIND - 1 ]

cmd="$1"

# if [ $OPTIND -lt 1 ]
#   then echo "Usage: ./install.sh [options]"
#        exit -1
# fi

# [ ( $show_version -eq 1 ) -o ( "$cmd" == "version" ) ]
if [ "$cmd" == "version" ]
  then 
       if [ -z "${SGP_ROOT}" ]
	 then echo "SGP_ROOT undefined; do sgp_admin init to initialise SymGrid-Par"
              exit 1
         else echo "SymGrid-Par version $SGP_VERSION"
              exit 0
       fi      
fi

# if [ ( $help -eq 1 ) -o ( "$cmd" == "help" ) ]
if [ "$cmd" == "help" ]
 then no_of_lines=`cat $0 | awk 'BEGIN { n = 0; } \
                                 /^$/ { print n; \
                                        exit; } \
                                      { n++; }'`
      echo "`head -$no_of_lines $0`"
      exit 
fi

if [ $verbose -eq 1 ]
 then echo "Command: $cmd"
      echo "Determining architecture: $hwos_str"
fi

# INIT=`pwd`/sgp_init.sh
# INIT=${SGP_ROOT}/../sgp_init.sh
INIT=sgp_init.sh

if [ "$cmd" == "init" ] 
  then # source $INIT
       echo "Command init not supported; do 'source $INIT' on the command line"
       exit 0
fi

# for all other commands, we need SGP_ROOT etc
if [ -z "${SGP_ROOT}" ]
 then echo "SGP_ROOT undefined; do sgp_admin init to initialise SymGrid-Par"
      exit 1
fi

if [ -z "$cmd" ] 
  then echo "No command supplied"
       echo "Usage: sgp_admin <cmd> [options]"
       exit 1
fi

if [ "$cmd" == "launch" ]
 then 

file="$2"

if [ -z "$file" ] 
 then 
    if [ -z "${SGPRC}" ]
    then echo "SGPRC undefined"
	exit 1
    else sgprc="$SGPRC"
    fi
 else
    sgprc="$file"
    export SGPRC="$file"
    # echo "sgprc: $sgprc"
fi

ports="`cat $sgprc | awk '{ print $2 }'`"

# echo "Ports: $ports"
pushd ${SGP_ROOT}/include

for p in $ports 
do
  gapd.sh -p $p -t ${SGP_ROOT}/include/sgp_server.g
done

popd

exit 0

fi # launch

if [ "$cmd" == "start" ]
 then 

pvm_stop
pvm_start

if [ ! -z "$2" ]
    then sgprc="$2"
    else if [ -z "${SGPRC}" ]
           then echo "SGPRC undefined"
                exit 1
           else sgprc="${SGPRC}"
         fi
fi
# for now we only launch the CoordinationServer
CoordinationServer_pp --sgprc ${sgprc} ${cs_port} +RTS -qp${SGP_NOPES:-2} &

exit 0

fi

if [ "$cmd" == "kill" ]
 then 

user="`whoami`"
# kill CoordinationServer
procs="`ps -u $user | fgrep ${user}=Coordin | awk '{ print $1 }' | sed ':a;N;\$!ba;s/\n/ /g' `"
if [ ! -z "$procs" ] 
  then kill -9 $procs
fi
# shut down pvm
pvm_stop
# kill running gap instances
procs="`ps -u $user | fgrep gap | awk '{ print $1 }' | sed ':a;N;\$!ba;s/\n/ /g' `"
if [ ! -z "$procs" ] 
  then kill -9 $procs
fi

exit 0

fi # kill

echo "Unknown command $1"
exit 1


