# -----------------------------------------------------------------------------
# GHC-Maple interface
#
# (c) 2000
#     Wolfgang Schreiner <Wolfgang.Schreiner@risc.uni-linz.ac.at>
#     Hans-Wolfgang Loidl <hwloidl@cee.hw.ac.uk>
#
#
# parMapReduce
# (c) 2008
#     A. Al Zain <ceeatia@macs.hw.ac.uk> 
#
# -----------------------------------------------------------------------------
#
#
GHC = /u1/staff/ceeatia/bin/Eden-6.x
#for Debug
##GHC_CFLAGS=-fth -cpp -parpvm -hisuf 'parpvm_hi' -osuf 'parpvm_o' -debug -fglasgow-exts -O -i$(gapROOTEden)
GHC_CFLAGS=-fth -cpp -parpvm -hisuf 'parpvm_hi' -osuf 'parpvm_o' -fglasgow-exts -O -i$(casROOTEden)
GHC_LFLAGS=-L$(casROOTEden) -lghccas -package parallel -package template-haskell -parpvm

all : parReduce

parReduce: parReduce.parpvm_o 
	$(GHC) $(GHC_LFLAGS) parReduce.parpvm_o -o parReduce

parReduce.parpvm_o: parReduce.hs
	$(GHC) $(GHC_CFLAGS) -c parReduce.hs


clean:
	rm *~ parReduce *.parpvm*

