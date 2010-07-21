APP_FILES := $(wildcard ../lib/*/ebin)
APPS := $(APP_FILES:../lib/%/ebin=%)

apps: $(APPS:%=app_%)
app_%:
	cd $*; make app

clean: $(APPS:%=clean_%)
clean_%:
	cd $*; make clean
