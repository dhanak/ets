# This is in fact a -*- makefile -*-.
GUTS_ROOT ?= $(abspath $(dir $(firstword $(MAKEFILE_LIST)))/..)
include $(GUTS_ROOT)/Makefile.common

# set up required tools
$(eval $(call set-up-tools-template,ELIXIRC,elixirc))
$(eval $(call set-up-tools-template,LN,ln))

SOURCES := eval.ex $(filter-out Makefile.ex eval.ex,$(wildcard *.ex))
TARGETS := $(shell echo $(SOURCES) | \
	sed -E -e 's/(\b|_)([a-z])/\U\2/g' -e 's/([^ ]+)\.Ex/Elixir.\1.beam/g')

default: all

.PHONY: all
#: Build all targets
all: $(TARGETS)

.PHONY: clean
#: Remove all build files
clean:
	$(RM) $(TARGETS)
	[ -L eval.ex ] && $(RM) eval.ex

%.beam: | $(ELIXIRC)
	$(ELIXIRC) $^

eval.ex: | $(LN)
	$(LN) -sf $(GUTS_ROOT)/eval/$@

.depend: $(SOURCES)
	@for f in $(SOURCES); do \
		echo -n "$$f" | sed -E -e 's/(^|_)([a-z])/\U\2/g' -e 's/(.+)\.ex/Elixir.\1.beam/g'; \
		echo ": $$f"; \
	done >.depend

include .depend
