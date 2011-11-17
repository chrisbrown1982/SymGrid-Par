# Time-stamp: <Thu Apr 15 2010 11:39:02 Stardate: Stardate: [-28]2922.21 hwloidl>
# -----------------------------------------------------------------------------
# SymGrid-Par
#
# (c) 03/2010 Hans-Wolfgang Loidl (hwloidl@cs.st-andrews.ac.uk)
# (c) 01/2009 Jost Berthold (jb@cs.st-andrews.ac.uk)
#
# Makefile for SCSCP interface and coordination server.
#
# This makefile defines how to build the main components of the SymGrid-Par
# infrastructure, in particular the coordination server.
#
# The SCSCP interface builds in the HaXmL for GHC: 
#   http://www.haskell.org/HaXml/
# We do not rely on an existing HaXML installation, however, and provide the
# generated .hs files as part of the distribution. Search for SCSCP.dtd on how
# to generate the .hs from the .dtd. 
#
# Environment-specific aspects of the system are defined in: paths.mk
# You MUST update the first section in paths.mk before building the system.
#
# Naming conventions: 
#  _pp suffix indicates a parallel program using PVM (from *.parpvm_{o,hi})
#  _pm suffix indicates a parallel program using MPI (from *.parmpi_{o,hi})
#	
# For debugging, setup the DEBUG flag.
#
# Targets:
#  make all ... builds SCSCP library, servers and clients
#  make binary-dist .. wraps-up a distribution in subdir dist/
# -----------------------------------------------------------------------------

# this file is included everywhere...
include ../paths.mk

OTHERSRCS  = SCSCP.dtd tweakDtd.patch 
LITSRCS    = HS_SCSCP.lhs
LIBSRCS    = Date.hs SCSCP_DTD.hs ParSCSCP.hs HS2SCSCP.hs SCSCP_API.hs
SERVERSRCS = BaseServices.hs CA_Skeletons.hs Monitor.hs SCSCP_Examples.hs HaskellServer.hs CoordinationServer.hs
CLIENTSRCS = testClient.hs
SRCS= $(LIBSRCS) $(LITSRCS) $(SERVERSRCS) $(CLIENTSRCS)

IMPORTS = $(subst .lhs,.hi,$(LITSRCS)) $(subst .hs,.hi,$(filter-out ParSCSCP.hs,$(LIBSRCS))) $(subst .hs,.parpvm_hi,$(LIBSRCS))
LIBOBJS = $(subst .lhs,.o,$(LITSRCS)) $(subst .hs,.o,$(filter-out ParSCSCP.hs,$(LIBSRCS)))
SERVEROBJS = $(subst .hs,.o,$(SERVERSRCS))
CLIENTOBJS = $(subst .hs,.o,$(CLIENTSRCS))

# SCSCP servers provided by SymGrid-Par
SERVERS = HaskellServer CoordinationServer_pp
CLIENTS = testClient
LIBRARY = libghc_cas_scscp.a

# computer algebra system running as SCSCP server
CASSERVER = $(shell ls GAP/*.g)


# EXTRAFLAGS += -Icbits -optl-Lcbits
EXTRAFLAGS += -i/scratch/chris/release/edenmodules -i../GCA/lib-CAS/Eden -Icbits -optl-Lcbits -XFlexibleContexts
# this is needed if you want to use edenTV for trace viewing:
# -L/home/jb/infrastructure/Pablo/lib 
# for debugging add: 
# -DDEBUG

# C compiler for cbits part:
CC_FLAGS = -Wall -O #  -D__PARALLEL_HASKELL__ -debug -DDEBUG

# Haskell compiler, compiling Eden code. Cleaned and -O2 added by JB 
GHC_FLAGS = -cpp -O2 $(EXTRAFLAGS) 

# for debug add -debug -DDEBUG, and switch off -O2 by -O0
# use like this: %>make DEBUG=yes
ifeq ("x$(DEBUG)","xyes") 
DBGFLAGS = -DDEBUG 
GHC_FLAGS += -debug -DDEBUG -O0
CC_FLAGS += -DDEBUG
endif

# flags for compilation way, here PVM-parallel, and package flag for strategies (parallel-1.xx)
PARFLAGS=-parpvm -hisuf parpvm_hi -osuf parpvm_o -package parallel cbits/monitor.o
# flags for compilation way, here MPI-parallel, and package flag for strategies (parallel-1.xx)
# PARFLAGS=-parmpi -hisuf parmpi_hi -osuf parmpi_o -package parallel cbits/monitor.o

# CFLAGS=$(PARFLAGS) -O2 $(IFLAGCAS) $(EXTRAFLAGS) $(DEBUGFLAGS)
# LFLAGS=$(LFLAGCAS) $(PARFLAGS) $(EXTRAFLAGS) $(DEBUGFLAGS)

# -----------------------------------------------------------------------------
# main target

all:  $(SERVERS) $(CLIENTS) $(LIBRARY)

# -----------------------------------------------------------------------------

Date.hs:
	@echo "module Date where" > Date.hs
	@echo "date =  \", "`date +"%A %d %B %Y, %H:%M"`"\"" >> Date.hs
	@echo "sys_name =  \"SymGrid-Par\"" >> Date.hs
	@echo "release =  \" $(RELEASE)\"" >> Date.hs
	@echo "root =  \" $(PWD)\"" >> Date.hs
	@echo "sgp_root =  \" $(SGP_ROOT)\"" >> Date.hs
	@echo "hw_os =  \" $(HWOS)\"" >> Date.hs
	@echo "host_name =  \" $(HOSTNAME)\"" >> Date.hs
	@echo "arch =  \" $(ARCH)\"" >> Date.hs
	@echo "stardate =  \" $(STARDATE)\"" >> Date.hs

SCSCP_API.o: Monitor.o $(LIBSRCS) SCSCP_DTD.hs
	$(EDEN_GHC) $(GHC_FLAGS) --make SCSCP_API -c 

ParSCSCP.o: Monitor.o $(LIBSRCS) SCSCP_DTD.hs
	$(EDEN_GHC) $(GHC_FLAGS) $(PARFLAGS) --make ParSCSCP.hs -c 

HaskellServer: HaskellServer.hs $(LIBSRCS) $(SERVERSRCS) SCSCP_DTD.hs
	$(EDEN_GHC) $(GHC_FLAGS) --make HaskellServer.hs -o HaskellServer

testClient: testClient.hs $(SRCS) SCSCP_DTD.hs
	$(EDEN_GHC) $(GHC_FLAGS) --make testClient.hs -o testClient

CoordinationServer: CoordinationServer.hs $(SRCS) cbits/monitor.o CoordinationServer.hs
	$(EDEN_GHC) $(GHC_FLAGS) --make CoordinationServer.hs -o CoordinationServer

CoordinationServer_pp: CoordinationServer.hs $(SRCS) cbits/monitor.o CoordinationServer.hs
	$(EDEN_GHC) $(GHC_FLAGS)  $(PARFLAGS) --make CoordinationServer.hs -o CoordinationServer_pp

testClient_pp: testClient.hs $(SRCS) SCSCP_DTD.hs
	$(EDEN_GHC) $(GHC_FLAGS) $(PARFLAGS) --make testClient.hs  -o testClient_pp

# SCSCP_DTD.hs can be generated automatically; needs HaXML; for now we just use the existing .hs file
# SCSCP_DTD.hs: SCSCP.dtd tweakDtd.patch
# 	DtdToHaskell SCSCP.dtd SCSCP_DTD.hs
# 	patch SCSCP_DTD.hs tweakDtd.patch

cbits/monitor.o: cbits/monitor.c cbits/monitor.h
	gcc $(CC_FLAGS) -Icbits -c cbits/monitor.c -o cbits/monitor.o 

# we will need to include cbits in Monitor later
Monitor.o : cbits/monitor.o cbits/monitor.h Monitor.hs
	$(EDEN_GHC) $(GHC_FLAGS) -icbits -optc-Icbits -c cbits/monitor.o Monitor.hs  

clean: parclean
	-rm *^ *~
	-rm $(LIBRARY) $(SERVERS) $(CLIENTS) $(LIBOBJS) $(SERVEROBJS) $(CLIENTOBJS)
	-rm -f cbits/monitor.*o

veryclean:  parclean
	rm -fr *.o  *.hi
#	rm -f SCSCP_DTD.hs
	rm -f cbits/monitor.*o

# this is for development only and probably doesn't work now
# run API with GHCI. This requires to interpret the modules, otherwise
# ghci-error with ghc-6.8.3.20080718-eden. -fforce-recomp does the
# job.
# requirement SCSCP_API.o triggers a prior compilation 

runAPI:	$(SRCS) SCSCP_API.o 
	$(EDEN_GHC) --interactive -fforce-recomp SCSCP_API ${DBGFLAGS}

cash: 
	ghci $(EXTRAFLAGS) Cash.hs

# -----------------------------------------------------------------------------
# parallel stuff, separated: 
# used for parallel version... separate for now.

PAR_FLAGS= -parpvm -hisuf 'parpvm_hi' -osuf 'parpvm_o'
EDENMODULES=./Eden

PAR_GHC=$(EDEN_GHC) $(GHC_FLAGS) $(PAR_FLAGS) -i$(EDENMODULES)

par: 	Monitor.parpvm_o ParSCSCP.parpvm_o

ParSCSCP.parpvm_o: 	SCSCP_DTD.hs ParSCSCP.hs Monitor.parpvm_o $(SRCS) $(EDENMODULES)/Eden.parpvm_o
	$(PAR_GHC) --make ParSCSCP.hs -c

$(EDENMODULES)/Eden.parpvm_o: $(EDENMODULES)/Eden.hs $(EDENMODULES)/ParPrim.hs 
	$(PAR_GHC) --make -c $(EDENMODULES)/Eden.hs

$(EDENMODULES)/Edi.parpvm_o: $(EDENMODULES)/Edi.hs $(EDENMODULES)/ParPrim.hs
	$(PAR_GHC) --make -c $(EDENMODULES)/Edi.hs 

# Skeleton library target, not needed at the moment:
$(EDENMODULES)/Skeletons.parpvm_o: $(EDENMODULES)/Skeletons.lhs $(EDENMODULES)/Edi.hs $(EDENMODULES)/EdiWP.hs $(EDENMODULES)/Eden.hs $(EDENMODULES)/ParPrim.hs $(EDENMODULES)/ParMap.hs 
	$(PAR_GHC) --make -c $(EDENMODULES)/Skeletons.lhs 

cbits/monitor.parpvm_o: cbits/monitor.o
	cp  cbits/monitor.o  cbits/monitor.parpvm_o

# we will need to include cbits in Monitor later
Monitor.parpvm_o : cbits/monitor.parpvm_o cbits/monitor.h Monitor.hs
	$(PAR_GHC) -icbits -optc-Icbits -c cbits/monitor.parpvm_o Monitor.hs  

# -----------------------------------------------------------------------------

libghc_cas_scscp.a:  $(LIBOBJS)
	ar r $@ $?

# -----------------------------------------------------------------------------

binary-dist:
	cp $(SERVERS) $(CLIENTS) $(DISTDIR)/bin
	cp $(LIBRARY) $(DISTDIR)/lib
	cp $(CASSERVER) $(DISTDIR)/lib
	-cp $(IMPORTS) $(DISTDIR)/imports

include ../install.mk 

# see ../install.mk
# install: all
# 	cp $(SERVERS) $(CLIENTS) $(INSTALLDIR)/bin
# 	cp $(LIBRARY) $(INSTALLDIR)/lib
# 	cp $(CASSERVER) $(INSTALLDIR)/lib
# 	cp $(IMPORTS) $(INSTALLDIR)/imports

parclean:
	rm -f *.parpvm_o *.parpvm_hi
	rm -f $(EDENMODULES)/*.parpvm_o $(EDENMODULES)/*.parpvm_hi
	rm -f cbits/*.parpvm_o
