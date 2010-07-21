-include Dependencies

APP_DIR := ..
EBIN_DIR := $(APP_DIR)/ebin
INCLUDE_DIR := $(APP_DIR)/include

ERL := erl
ERLC := erlc
ERLC_FLAGS := $(ERLC_OPTS)

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
	$(ERL) $(ERL_OPTS) -pa $(EBIN_DIR) -noshell -s $(TESTS) test -s init stop

shell: $(ERL_OBJECTS)
	$(ERL) $(ERL_OPTS) -pa $(EBIN_DIR) $(SHELL_OPTS)

clean:
	rm -rf $(EBIN_DIR)/*.beam
