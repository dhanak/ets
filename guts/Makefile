include $(dir $(firstword $(MAKEFILE_LIST)))/Makefile.common

# set up required tools
$(eval $(call set-up-tools-template,MYSQL_CONFIG,mysql_config))
$(eval $(call set-up-tools-template,SICSTUS,sicstus))
$(eval $(call set-up-tools-template,SPLD,spld))
$(eval $(call set-up-tools-template,SPLFR,splfr))
$(eval $(call set-up-tools-template,STRIP,strip))

# Set variables
MYSQL_CFLAGS := $(shell $(MYSQL_CONFIG) --cflags)
MYSQL_LDFLAGS := $(shell $(MYSQL_CONFIG) --libs)
PROLOG_SOURCES := $(wildcard *.pl)
TARGETS := guts timeguard

# check for error and warning lines and signal an error with exit code 1
SP_CHECK = 2>&1 | awk '{print} /^[!*]/{ec=1} END{exit ec}'

default: all

.PHONY: all
#: Build all targets
all: $(TARGETS)

.PHONY: mrproper
#: Remove all generated files
mrproper: clean
	$(RM) $(TARGETS) guts.sav mysql.so

.PHONY: clean
#: Remove all intermediate build files
clean:
	$(RM) *.o

#: Build guts binary
guts: guts.sav mysql.so | $(SPLD) $(STRIP)
	$(SPLD) -o $@ $< ${SP_CHECK}
	$(STRIP) $@

#: Build timeguard binary
timeguard: timeguard.c | $(STRIP)
	$(CC) -O3 -Wall -Wextra -Werror -o $@ $<
	$(STRIP) $@

guts.sav: $(PROLOG_SOURCES) mysql.so | $(SICSTUS)
	$(SICSTUS) -f --noinfo --nologo \
		--goal "compile('guts'),save_program('$@'),halt." ${SP_CHECK}

mysql.so: mysql.pl mysql.c | $(MYSQL_CONFIG) $(SPLFR)
	$(SPLFR) $^ --cflag='-Wall -Wextra -Werror $(MYSQL_CFLAGS)' \
		-LD $(MYSQL_LDFLAGS) ${SP_CHECK}
