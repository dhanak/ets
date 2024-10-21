# This is in fact a -*- makefile -*-.
GUTS_ROOT ?= $(abspath $(dir $(firstword $(MAKEFILE_LIST)))/..)
include $(GUTS_ROOT)/Makefile.common

# set up required tools
$(eval $(call set-up-tools-template,SICSTUS,sicstus))

SOURCES := $(filter-out Makefile.pl,$(wildcard *.pl))
TARGETS := $(patsubst %.pl,%.po,$(SOURCES))

.SUFFIXES: .pl .po

default: all

.PHONY: all
#: Build all targets
all: $(TARGETS)

.PHONY: clean
#: Remove all build files
clean:
	$(RM) *.po
	$(RM) $(TARGETS)

%.po: %.pl | $(SICSTUS)
	$(SICSTUS) -f --noinfo --nologo \
		--goal "catch((compile('$<'),save_files('$<','$@')),_,halt(1))."
