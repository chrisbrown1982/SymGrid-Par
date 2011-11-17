# -----------------------------------------------------------------------------
# GHC-Maple interface
#
# (c) 2000
#     Wolfgang Schreiner <Wolfgang.Schreiner@risc.uni-linz.ac.at>
#     Hans-Wolfgang Loidl <hwloidl@cee.hw.ac.uk>
#
#
# masterWorkers Skeletons 
# (c) 2008
#     A. Al Zain <ceeatia@macs.hw.ac.uk>
# -----------------------------------------------------------------------------
#
GHC = /u1/staff/ceeatia/bin/Eden-6.x

GHC_CFLAGS=-fth -cpp -parpvm -hisuf 'parpvm_hi' -osuf 'parpvm_o' -fglasgow-exts -O -i$(casROOTEden)
GHC_LFLAGS=-L$(casROOTEden) -lghccas -package parallel -package template-haskell -parpvm

all : masterSlavesMuPAD

masterSlavesMuPAD: masterSlavesMuPAD.parpvm_o 
	$(GHC) $(GHC_LFLAGS) masterSlavesMuPAD.parpvm_o -o masterSlavesMuPAD

masterSlavesMuPAD.parpvm_o: masterSlavesMuPAD.hs
	$(GHC) $(GHC_CFLAGS) -c masterSlavesMuPAD.hs


clean:
	rm *.*_o *~ *.*hi masterSlavesMuPAD *.parpvm*

checkin:
	echo 'To be implemented...'.

