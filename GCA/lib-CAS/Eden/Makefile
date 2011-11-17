# JB, 08/2006 - Part of IFL06 program suite
#    Customization for warshall program
###########################################

SRCS=*.hs
# EXTRA_OPTS = -no-hs-main

# special targets:
all	: Eden


Eden: NFData.hs ParPrim.hs Eden.hs EdenHelpers.hs FoldDM.hs
	$(EDEN_GHC) -D__PARALLEL_HASKELL__ -DDEBUG -debug $(WFLAG) $(EDEN_OPTS) --make $(EXTRA_OPTS) -o Eden6x_$(WAY) Eden.hs -no-recomp

# include generic version
include Makefile.com

