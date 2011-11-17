SUBDIRS = lib-CAS

all: 
	for d in $(SUBDIRS); do \
	  echo "== $(MAKE) all $(MFLAGS) in $(shell pwd)/$$d"; \
	  $(MAKE) --no-print-directory -C $$d $(MFLAGS) all; \
	done; \

clean: 
	for d in $(SUBDIRS); do \
	  echo "== $(MAKE) clean $(MFLAGS) in $(shell pwd)/$$d"; \
	  $(MAKE) --no-print-directory -C $$d $(MFLAGS) clean; \
	done; \

