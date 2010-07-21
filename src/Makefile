DEPENDS :=
# Set location of MOCHIWEB_APP as env var as needed
MOCHIWEB_APP := ../../mochiweb
ERL_OPTS := -pa $(MOCHIWEB_APP)/ebin
ERLC_OPTS := -DTEST
TESTS := shark_tests
include ../make/app_src.mk
