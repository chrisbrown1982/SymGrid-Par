#
# generic Makefile for Eden with GHC 5.00.2
# Make-Mode of GHC simplifies very much...
# 
# The only thing to care about is 
#  to have the file names exactly identical
#  to the module names which are imported.
###########################################
# the compiler

# can be set using $>gmake EDEN_GHC=/path/to/my/eden/ghc 
ifeq ($(strip $(EDEN_GHC)),) 
#  EDEN_GHC=/export/tmp/berthold/test/eden6/ghcBLDst2
#  EDEN_GHC=/u1/others/berthold/bin/ghc6-eden
   EDEN_GHC=/u1/staff/ceeatia/bin/Eden-6.x

endif

# to be set when compiling with gmake: gmake WAY=parpvm/parmpi
ifeq ($(strip $(WAY)),)
  WAY=parpvm
endif

#eden options: should be included for every Eden-compilation
# CAREFUL WITH OPTIMIZATION !
#EDEN_OPTS= -hisuf '$(WAY)_hi' -osuf '$(WAY)_o'
EDEN_OPTS= -hisuf '$(WAY)_hi' -osuf '$(WAY)_o' -O2

# other things, can be overwritten by children...
ifndef MAIN
  MAIN=Main
endif

# no flag if sequential version
ifeq "$(strip $(WAY))" "seq"
  WFLAG=
else
  WFLAG=-$(WAY)
endif

# other source files can be defined...
ifndef SRCS
  SRCS=*hs
endif

$(MAIN)_$(WAY) 	: $(SRCS)
	$(EDEN_GHC) $(WFLAG) $(EDEN_OPTS) --make $(EXTRA_OPTS) -o $(MAIN)_$(WAY) $(MAIN).hs

.PHONY	: allclean
allclean	:
	 -rm -f *o *hi $(MAIN)_*

.PHONY	: clean
clean	: 
	 -rm -f *.$(WAY)_o *.$(WAY)_hi 
