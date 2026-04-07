# preset    ?= arm-none-eabi
preset ?= linux-debug
main      ?=
ctest     ?= ctest
cmake     ?= cmake
generator ?= "Unix Makefiles"
memory_logging ?= OFF

cmake_configure_options := -G $(generator) --preset $(preset) -DTPL_MEMORY_LOGGING=$(memory_logging)

build_dir := build/$(preset)
bin       := $(build_dir)/tpl


info:
	@echo "preset     = $(preset)"
	@echo "build_dir   = $(build_dir)"
	@echo "binary      = $(bin)"
	@echo "main (opt)  = $(main)"

configure:
	$(cmake) $(cmake_configure_options)

configure-compile:
	$(cmake) $(cmake_configure_options) -DMAIN_PL="$(main)"

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

generate-header-stubs: clean-all
	rm build/arm-none-eabi-compile -rf
	cmake --preset arm-none-eabi-compile -DCOMPILE_DIAGNOSTICS_FORMAT=text
	./tools/gen-compat-stubs cmake --build --preset arm-none-eabi-compile -- -k -j$(nproc)

generate-report:
	rm build/arm-none-eabi-compile -rf
	cmake --preset arm-none-eabi-compile -DCOMPILE_DIAGNOSTICS_FORMAT=json-file
	cmake --build --preset arm-none-eabi-compile -- -k -j1 || true
	./tools/create-report build/arm-none-eabi-compile

linux-debug:
	$(MAKE) rebuild preset=linux-debug

linux-release:
	$(MAKE) rebuild preset=linux-release

baremetal:
	$(MAKE) rebuild preset=baremetal-arm-none-eabi

compile-commands:
	@test -f "$(build_dir)/compile_commands.json" && \
	  echo "$(build_dir)/compile_commands.json" || \
	  (echo "missing: $(build_dir)/compile_commands.json (configure first)" && exit 1)


cache:
	$(cmake) -LA -N "$(build_dir)" | sort

.PHONY: default help info \
        configure configure-compile build rebuild \
        run test test-sh check leaks \
        compile run-compile \
        clean clean-all \
        debug release cross baremetal \
        compile-commands cache \
		generate-header-stubs
