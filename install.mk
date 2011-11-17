# Time-stamp: <Thu Jul 01 2010 14:51:37 Stardate: Stardate: [-28]3307.67 hwloidl>
#
# Makefile of source and binary distributions of SymGrid-Par.
# This file should be in the top-level dir of a bin dist.
# For now all we do is to install the binaries in $SGP_ROOT.
# -----------------------------------------------------------------------------

BIN_DIST_DIRS = bin lib	share include doc imports

install:
	install -c src/CoordinationServer_pp src/testClient $$SGP_ROOT/bin
	install -c src/libghc_cas_scscp.a $$SGP_ROOT/lib

install-from-bin: binary-dist
	-if [ -z "$$SGP_ROOT" ] ; then echo "SGP_ROOT undefined" ; exit ; fi
	-if [ -d $(BIN_DIST_NAME)/dist ] ; then cd $(BIN_DIST_NAME)/dist ; fi
	for d in $(BIN_DIST_DIRS); do \
	  install -c  $$d/* $$SGP_ROOT/$$d; \
	done 
