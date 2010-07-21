-include Dependencies.in
-include Dependencies

APP_DIR := ..
EBIN_DIR := $(APP_DIR)/ebin
INCLUDE_DIR := $(APP_DIR)/include
MOD := dummy

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

run: $(ERL_OBJECTS)
	$(ERL) $(ERL_OPTS) -pa $(EBIN_DIR) -s run_sample start $(MOD)

clean:
	rm -rf $(EBIN_DIR)/*.beam
