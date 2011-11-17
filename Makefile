# -----------------------------------------------------------------------------
# SymGrid-Par
#
# (c) SCIEnce project (http://www.symbolic-computation.org/The_SCIEnce_Project)
#
# Top level Makefile for the SymGrid-Par infrastructure.
#
# The real work is done in SCSCP/Makefile
# -----------------------------------------------------------------------------

# configuration stuff; this may need tuning
include paths.mk

# for now we build only the SCSCP-based interface
SUBDIRS = src

ALLFILES = README ANNOUNCE INSTALL USE Makefile paths.mk install.mk sgp_admin.sh sample-sgprc
ALLSRCS  = $(ALLFILES) CAG GCA SCSCP 

all: 
	for d in $(SUBDIRS); do \
	  echo "== $(MAKE) all $(MFLAGS) in $(shell pwd)/$$d"; \
	  $(MAKE) --no-print-directory -C $$d $(MFLAGS) all; \
	done; \

# target to install in $SGP_ROOT; re-used in bin_dist
include install.mk

# what kind of architecture is this?
hwos:
	@echo $(HWOS)

arch: hwos

clean: 
	-rm -fr dist
	-rm *^ *~
	for d in $(SUBDIRS); do \
	  echo "== $(MAKE) clean $(MFLAGS) in $(shell pwd)/$$d"; \
	  $(MAKE) --no-print-directory -C $$d $(MFLAGS) clean; \
	done; \

dist: clean
#	tar cvfz src.tgz Makefile paths.mk CAG GCA SCSCP 
	rm -f $(REL_NAME).tar.gz 
	mkdir $(REL_NAME)
#	tar cf - $(ALLSRCS) | ( cd $(REL_NAME); tar xf - ; find . \( -name .svn -a -type d \) -exec rm -rf \{\} \; )
	tar cf - --exclude '*/.svn*' $(ALLSRCS) | ( cd $(REL_NAME); tar xf - )
	tar cf $(REL_NAME).tar $(REL_NAME)
	rm -rf $(REL_NAME)
	gzip $(REL_NAME).tar

build-dist:
	if [ -d dist ] ; then rm -fr dist ; fi
	mkdir dist
	for d in $(BIN_DIST_DIRS); do \
	  mkdir dist/$$d; \
	done 

binary-dist: all build-dist
	for d in $(SUBDIRS); do \
	  echo "== $(MAKE) binary-dist $(MFLAGS) in $(shell pwd)/$$d"; \
	  $(MAKE) --no-print-directory -C $$d $(MFLAGS) binary-dist; \
	done; 
	rm -f $(BIN_DIST_NAME).tar.gz
	if [ -d $(BIN_DIST_NAME) ] ; then rm -fr $(BIN_DIST_NAME) ; fi
	mkdir $(BIN_DIST_NAME)
	cp install.mk dist/Makefile
	cp $(ALLFILES) dist/.
#	tar cf - dist | ( cd $(BIN_DIST_NAME); tar xf - ; find . \( -name .svn -a -type d \) -exec rm -rf \{\} \; )
	tar cf - --exclude '*/.svn*' dist | ( cd $(BIN_DIST_NAME); tar xf - )
	tar cf $(BIN_DIST_NAME).tar $(BIN_DIST_NAME)
	rm -rf $(BIN_DIST_NAME)
	gzip $(BIN_DIST_NAME).tar

