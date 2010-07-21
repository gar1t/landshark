TOP_DIR := ../../..
LIB_DIR := $(TOP_DIR)/lib
APP_DIR := ..
EBIN_DIR := $(APP_DIR)/ebin
INCLUDE_DIR := $(APP_DIR)/include

EBIN_DIRS := $(EBIN_DIR) $(DEPENDS:%=$(LIB_DIR)/%/ebin)
INCLUDE_DIRS := $(INCLUDE_DIR) $(LIB_DIR) $(DEPENDS:%=$(LIB_DIR)/%/include)

ERL := erl
ERLC := erlc
ERLC_FLAGS := -W $(INCLUDE_DIRS:%=-I %) $(EBIN_DIRS:%=-pa %) $(ERLC_OPTS)

ifndef no_debug_info
  ERLC_FLAGS += +debug_info
endif

ifdef debug
  ERLC_FLAGS += -Ddebug
endif

ERL_SOURCES := $(wildcard *.erl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.beam)

$(EBIN_DIR)/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

app:  $(ERL_OBJECTS)

release: app

test: $(ERL_OBJECTS)
	ERL_LIBS=$(LIB_DIR) $(ERL) -noshell -s $(TESTS) test -s init stop

shell: $(ERL_OBJECTS)
	ERL_LIBS=$(LIB_DIR) $(ERL) $(SHELL_OPTS)

clean:
	rm -rf $(EBIN_DIR)/*.beam
