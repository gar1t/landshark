TOP_DIR := .
LIB_DIR := $(TOP_DIR)/lib
REL_ROOT := $(TOP_DIR)/releases
REL_DIR := $(REL_ROOT)/$(RELEASE)
ERL := erl

REL_SOURCES := $(wildcard $(REL_DIR)/*.rel)
BOOT_OBJECTS := $(REL_SOURCES:%.rel=%.boot)

$(REL_DIR)/%.boot: $(REL_DIR)/%.rel
	ERL_LIBS=$(LIB_DIR) $(ERL) -noshell \
		-s systools make_script $(basename $<) \
		-s init stop

releases: apps $(BOOT_OBJECTS)

clean-releases:
	rm -f $(REL_DIR)/*.boot
	rm -f $(REL_DIR)/*.script

apps:
	cd lib; make apps

clean: clean-releases
	cd lib; make clean

