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
GHC_CFLAGS=-fth -cpp -parpvm -hisuf 'parpvm_hi' -osuf 'parpvm_o' -fglasgow-exts -O -i$(gapROOTEdenSCSCP)
GHC_LFLAGS=-L$(gapROOTEdenSCSCP) -lghcgap -package unix -package template-haskell -parpvm

all : masterSlaves

masterSlaves: masterSlaves.parpvm_o 
	$(GHC) $(GHC_LFLAGS) masterSlaves.parpvm_o -o masterSlaves_pp

masterSlaves.parpvm_o: masterSlaves.hs
	$(GHC) $(GHC_CFLAGS) -c masterSlaves.hs


clean:
	rm *.*_o *~ *.*hi masterSlaves_pp *.parpvm*

checkin:
	echo 'To be implemented...'.

