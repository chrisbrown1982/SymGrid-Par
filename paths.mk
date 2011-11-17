# Time-stamp: <Wed Jun 16 2010 20:38:55 Stardate: Stardate: [-28]3234.09 hwloidl>
# -----------------------------------------------------------------------------
# SymGrid-Par
#
# (c) 03/2010 Hans-Wolfgang Loidl (hwloidl@cs.st-andrews.ac.uk)
# (c) 01/2009 Jost Berthold (jb@cs.st-andrews.ac.uk)
#
# SCIEnce project: CAG/GCA interface
# 
# Configuration of CA system paths (and other var.s)
#
#####################################################

# configuration variables indicating the package location 
# this is needed/set when building the library, needed when
# linking the examples

#todo:	
#	echo "please correct paths in paths.mk"
#	echo "Then you can delete the todo target on top"
#	exit 1

# TODO: automatic configure and building a ghc -package ghc_cas

# adjust this to the full path on your system
GHCCAS=$(PWD)/lib-CAS
# if you work with a SVN checkout, you can use something like this
#GHCCAS=/home/hwloidl/science/jra/systems/SymGrid-Par/lib-CAS

# for programs, this is the location where libghccas.a and all
# interfaces (*hi) are located. This library will provide XxxAPI.hs
# and Eden/Edi lib.s. Eden stuff is separated in a subdirectory Eden/

# use same compiler for example programs and library:
# can be set using $>gmake EDEN_GHC=/path/to/my/eden/ghc 
ifeq ($(strip $(EDEN_GHC)),) 
# EDEN_GHC=ghc-6.8.3.20080718-eden
# EDEN_GHC=ghc-6.12.2-eden
EDEN_GHC=ghc
endif

# sequential GHC; should only be needed for debugging the system
ifeq ($(strip $(GHC)),) 
GHC=ghc-6.4.1
endif

#####################################################

# Variables for CA systems, not used outside library

# used for starting gap, by $(GAPROOT)/bin/gap.sh
# this is on ardbeg. Above for development, below for release

ifeq ($(strip $(GAPROOT)),) 
GAPROOT=$(SGP_ROOT)/gap4r4
endif

########################################################
########################################################
#########                                       ########
######### Below this point, no editing should   ########
######### be necessary. TODO migrate settings   ########
######### below to Makefiles where they belong. ########
#########                                       ########
########################################################
########################################################

#MAPLEROOT=/usr/local/maple/ # but in ardbeg, it is...
MAPLEROOT=/usr/local/maple11

# on ardbeg
MUPADROOT=/opt/MuPAD-4.0.2

# not found on ardbeg
KANTROOT=definitelyWrongKantPath


###################
# these definitions lived in the main Makefile, 
# now we include paths.mk instead
###################

# we have sort-of versioning... 

NAME=SymGrid-Par
RELEASE=0.3.3
HWOS=$(shell if [ -x "`which hw_os`" ] ; then echo `hw_os` ; else echo "UNKNOWN" ; fi )
HOSTNAME=$(shell hostname)
ARCH=$(shell uname -a)
STARDATE=$(shell if [ -x "`which stardate`" ] ; then echo "`stardate`" ; else echo "" ; fi )

REL_NAME=$(NAME)-v$(RELEASE)
BIN_DIST_NAME=$(REL_NAME)-$(HWOS)

# where to build the dir for a distribution
DISTDIR = $(PWD)/dist
# where to install it
INSTALLDIR = $(SGP_ROOT)


# However, abyd's example sez:
#GAP_FLAGS= -DGAPBIN="\"/home/ceeatia/bin/gap.sh\"" -DGAPARGS="\"-q -m 266m -o 1G\"" -DGAPENV="GAPROOT" 
# additionally, abyd's gap.sh is this:
# #!/bin/sh
# exec /usr/GAP/gap4r4/bin/gap.sh -q -m 266m -o 1G -L /home/ceeatia/GHC-GAP-0.2/examples/Eden/liouville/abydWorkspace
#

# Abyd sez: 

# -L is gap-space, starts up faster. a static file containing machine
# spec and such. Created in a gap session, by command SaveWorkspace(name);
# gap> a:=1;
# gap> SaveWorkspace("savefile");
# true
# gap> quit;
# http://www.gap-system.org/Manuals/doc/htm/ref/chapters.htm

# -o 1G is for memory issues... (Abyd: "did not help" :) )


MAPLE_FLAGS= -DMAPLEBIN="\"$$(MAPLEROOT)/bin/maple\"" -DMAPLEARGS="\"-q\"" -DGAPENV="MAPLEROOT" 

MUPAD_FLAGS= -DMUPADBIN="\"wrongMuPadPath\""  -DMUPADARGS="\"-q\""  -DMUPADENV="MUPADROOT"
#MUPAD_FLAGS= -DMUPADBIN="\"//usr/local/bin/mupkern\""  -DMUPADARGS="\"-q\""  -DMUPADENV="MUPADROOT"

KANT_FLAGS= -DKANTBIN="\"definitelyWrongKantPath\""  -DKANTARGS=""  -DKANTENV="KANTROOT"
