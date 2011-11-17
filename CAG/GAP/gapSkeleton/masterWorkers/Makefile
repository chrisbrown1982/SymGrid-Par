# -----------------------------------------------------------------------------
# GHC-Maple interface
#
# (c) 2000
#     Wolfgang Schreiner <Wolfgang.Schreiner@risc.uni-linz.ac.at>
#     Hans-Wolfgang Loidl <hwloidl@cee.hw.ac.uk>
#
#
# masterWorkers
# (c) 2008
#     A. Al Zain <ceeatia@macs.hw.ac.uk> 
#
# -----------------------------------------------------------------------------
#
# To switch from Haskell to Eden
# set apropiate compiler.
#
# WARNING: Only tested on ghc-5.02.3 and Maple 7
# Different verions may fail !!!
#
GHC = /u1/staff/ceeatia/bin/Eden-6.x
#for Debug
##GHC_CFLAGS=-fth -cpp -parpvm -hisuf 'parpvm_hi' -osuf 'parpvm_o' -debug -fglasgow-exts -O -i$(gapROOTEden)
GHC_CFLAGS=-fth -cpp -parpvm -hisuf 'parpvm_hi' -osuf 'parpvm_o' -fglasgow-exts -O -i$(casROOTEden)
GHC_LFLAGS=-L$(casROOTEden) -lghccas -package parallel -package template-haskell -parpvm

all : masterWorkers

masterWorkers: masterWorkers.parpvm_o 
	$(GHC) $(GHC_LFLAGS) masterWorkers.parpvm_o -o masterWorkers

masterWorkers.parpvm_o: masterWorkers.hs
	$(GHC) $(GHC_CFLAGS) -c masterWorkers.hs


clean:
	rm *~ masterWorkers *.parpvm*

checkin:
	echo 'To be implemented...'.

