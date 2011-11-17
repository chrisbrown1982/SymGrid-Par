# -----------------------------------------------------------------------------
# GHC-GAP interface
#
# (c) 2006
#     A. D. Al Zain <ceeatia@macs.hw.ac.uk>
#
#
# (c) 2000
#     Wolfgang Schreiner <Wolfgang.Schreiner@risc.uni-linz.ac.at>
#     Hans-Wolfgang Loidl <hwloidl@cee.hw.ac.uk>
#
#
#  Usage:
#    - Change GAP_FLAGS to apropiate values.
#    - To switch from Haskell to GUM, you should: 
#    	+ Set the apropiate compiler:
#
#	
#   For debugging, setup the DEBUG flag.
# -----------------------------------------------------------------------------
#
RELEASE=0.2
#
#GAP_FLAGS  = -DGAPBIN="\"/u1/staff/ceeatia/bin/gapdev\"" -DGAPARGS="\"-q -r -A -m 100m -o 0.5G\"" -DGAPENV="GAPROOTl"  #This supports OM
GAP_FLAGS  = -DGAPBIN="\"/u1/staff/ceeatia/bin/gap.sh\"" -DGAPARGS="\"-q -r -A\"" -DGAPENV="GAPROOT" #This is for GAP without OM
#GAP_FLAGS  = -DGAPBIN="\"/u1/staff/ceeatia/bin/runGap\"" -DGAPARGS="" -DGAPENV="GAPROOT" #This is for GAP without OM
MAPLE_FLAGS= -DMAPLEBIN="\"/usr/local/maple/bin/maple\"" -DMAPLEARGS="\"-q\"" -DGAPENV="MAPLEROOT" 
MUPAD_FLAGS= -DMUPADBIN="\"/u1/staff/ceeatia/bin/runMupad\""  -DMUPADARGS="\"-q\""  -DMUPADENV="MUPADROOT"
KANT_FLAGS= -DKANTBIN="\"/u1/staff/ceeatia/bin/runKant\""  -DKANTARGS=""  -DKANTENV="KANTROOT"

#MUPAD_FLAGS= -DMUPADBIN="\"//usr/local/bin/mupkern\""  -DMUPADARGS="\"-q\""  -DMUPADENV="MUPADROOT"
#

# v0.3: if we use Eden then build everything with Eden
# TODO: support the old, bespoke interface using the CAS flags above
ifneq ($(strip $(EDEN_GHC)),) 
GHC=$(EDEN_GHC)
else
# Choose apropiate compiler
#GHC = /usr/local/ghc-5.02/bin/ghc
#GHC = /u1/staff/ceeatia/ghc_inst-5.02.3/bin/ghc
#GHC = /u1/staff/ceeatia/bin/my-gumO-qPg-stable-mpi
GHC = /u1/staff/ceeatia/bin/Eden-6.x
#GHC = /u1/others/berthold/bin/ghc6-eden
endif

# Set apropiate flags.
CC_FLAGS = -Wall -D__PARALLEL_HASKELL__ -debug -DDEBUG -O 
CompFlags = 
#GHC_FLAGS = -package unix -cpp -parpvm -hisuf 'parpvm_hi' -osuf 'parpvm_o' -debug -DDEBUG -D_PARALLEL_ -fglasgow-exts -fvia-C -i$(gapROOT02) -L$(gapROOT02) 

# for debug add -debug -DDEBUG
GHC_FLAGS = -fth -package parallel -cpp -parpvm -hisuf 'parpvm_hi' -osuf 'parpvm_o' -D__PARALLEL_HASKELL__ -fglasgow-exts -fvia-C -i$(casROOTEden) -L$(casROOTEden) -L$PABLO_ROOT/lib -lPabloTraceRC



all: libghc_cas.a

cbits/monitor2.o: cbits/monitor.c
	gcc $(CC_FLAGS) -c cbits/monitor.c -o cbits/monitor2.o 

Monitor.parpvm_o : cbits/monitor.c Monitor.hs
	$(GHC) $(GHC_FLAGS) -c cbits/monitor.c Monitor.hs  

gap.parpvm_o:   gap.hs 
	$(GHC)  $(GHC_FLAGS)  $(GAP_FLAGS) -O  -c gap.hs  

maple.parpvm_o:   maple.hs 
	$(GHC)  $(GHC_FLAGS)  $(MAPLE_FLAGS) -O  -c maple.hs  

mupad.parpvm_o: mupad.hs
	$(GHC)  $(GHC_FLAGS)  $(MUPAD_FLAGS) -O  -c mupad.hs 

kant.parpvm_o: kant.hs
	$(GHC)  $(GHC_FLAGS)  $(KANT_FLAGS) -O  -c kant.hs 

libghc_cas.a:  Edi.parpvm_o EdiWP.parpvm_o ParMap.parpvm_o Skeletons.parpvm_o Eden.parpvm_o NFData.parpvm_o ParPrim.parpvm_o cbits/monitor2.o Monitor.parpvm_o gap.parpvm_o maple.parpvm_o mupad.parpvm_o kant.parpvm_o
	make -C Eden all
	ar r $@ $?

checkin:
	cvs commit

clean:
	rm -fr *.o cbits/*.o *.a *.hi *~ *.parpvm*
	ln -s /u1/staff/ceeatia/GHC-GAP-0.2/.edenSkels/*parpvm* .

install: all
	cp libghcgap.a $(gapROOtEden)
	cp init.g.ori start.g $(gapROOTEden)
	touch $(gapROOT02/init.g
	cp gap.parpvm_hi $(gapROOTEden)/GAPAPI.parpvm_hi

