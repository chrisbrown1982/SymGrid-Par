#!/bin/sh 
export SGP_ROOT=`pwd`
export SGP_VERSION=0.3.3

export PATH=${SGP_ROOT}:${SGP_ROOT}/bin:$PATH

export PABLO_ROOT=/scratch/chris/SGP/txxi/SGP_v0.3.2 
export PVM_ROOT=/scratch/chris/SGP/txxi/SGP_v0.3.2_BUILDS/pvm3
export PVM_ARCH="LINUX64"
export LIBRARY_PATH=:/scratch/chris/SGP/txxi/SGP_v0.3.2_BUILDS/pvm3/lib/LINUX64
export MPIHOME=/scratch/chris/SGP/txxi/SGP_v0.3.2
alias ph="echo \"halt\" | pvm"
alias pq="echo \"quit\" | pvm"
alias pc="echo \"conf\" | pvm"
