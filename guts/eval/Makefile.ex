# This is in fact a -*- makefile -*-.
GUTS_ROOT ?= $(abspath $(dir $(firstword $(MAKEFILE_LIST)))/..)
include $(GUTS_ROOT)/Makefile.common

# set up required tools
$(eval $(call set-up-tools-template,ELIXIRC,elixirc))
$(eval $(call set-up-tools-template,LN,ln))

SOURCES := $(filter-out Makefile.ex,$(wildcard *.ex))
TARGETS := $(patsubst %.ex,%.beam,$(SOURCES))

.SUFFIXES: .ex .beam

default: all

.PHONY: all
#: Build all targets
all: $(TARGETS)

.PHONY: clean
#: Remove all build files
clean:
	$(RM) *.beam
	$(RM) $(TARGETS)

%.beam: %.ex | $(ELIXIRC)
	$(ELIXIRC) $<

.PHONY: depend
depend: | $(LN)
	$(LN) -sf $(GUTS_ROOT)/eval/eval.ex
