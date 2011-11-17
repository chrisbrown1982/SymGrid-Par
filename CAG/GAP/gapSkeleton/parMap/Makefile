# -----------------------------------------------------------------------------
# GHC-Maple interface
#
# (c) 2000
#     Wolfgang Schreiner <Wolfgang.Schreiner@risc.uni-linz.ac.at>
#     Hans-Wolfgang Loidl <hwloidl@cee.hw.ac.uk>
#
#
# Eden-Maple interface
# (c) 2003
#     Ricardo Peña-Marí <ricardo@sip.ucm.es>
#     Rafael Martinez-Torres <rmartine@fdi.ucm.es>
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

all : parMap

parMap: parMap.parpvm_o 
	$(GHC) $(GHC_LFLAGS) parMap.parpvm_o -o parMap

parMap.parpvm_o: parMap.hs
	$(GHC) $(GHC_CFLAGS) -c parMap.hs


clean:
	rm *~ parMap *.parpvm*

checkin:
	echo 'To be implemented...'.

