preset    ?= linux-debug
main      ?=
ctest     ?= ctest
cmake     ?= cmake
generator ?= "Unix Makefiles"

build_dir := build/$(preset)
bin       := $(build_dir)/tpl

.PHONY: default help info \
        configure configure-compile build rebuild \
        run test test-sh check leaks \
        compile run-compile \
        clean clean-all \
        debug release cross baremetal \
        compile-commands cache

info:
	@echo "preset     = $(preset)"
	@echo "build_dir   = $(build_dir)"
	@echo "binary      = $(bin)"
	@echo "main (opt)  = $(main)"

configure:
	$(cmake) -G $(generator) --preset $(preset)

configure-compile:
	$(cmake) -G $(generator) --preset $(preset) -DMAIN_PL="$(main)"

build:
	$(cmake) --build --preset $(preset) -- -j$(nproc)

rebuild: clean configure build

run:
	$(bin) $(ARGS)

test:
	$(ctest) --test-dir "$(build_dir)" --output-on-failure

test-sh:
	./tests/run.sh

check:
	./tests/run_valgrind.sh

leaks:
	./tests/run_valgrind_leaks.sh

compile: configure build

clean:
	rm -rf "$(build_dir)"

clean-all:
	rm -rf build

linux-debug:
	$(MAKE) rebuild preset=linux-debug

linux-release:
	$(MAKE) rebuild preset=linux-release

cross:
	$(MAKE) rebuild preset=cross-generic

baremetal:
	$(MAKE) rebuild preset=baremetal-arm-none-eabi

compile-commands:
	@test -f "$(build_dir)/compile_commands.json" && \
	  echo "$(build_dir)/compile_commands.json" || \
	  (echo "missing: $(build_dir)/compile_commands.json (configure first)" && exit 1)

cache:
	$(cmake) -LA -N "$(build_dir)" | sort
