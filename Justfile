default:
    @just --list

# Preset to use for all commands, run `just use <preset>` to change
# You can override it per-command using `just preset=<preset> [command]`
preset := `cat .preset 2>/dev/null || echo linux-debug`

use p:
    @echo '{{p}}' > .preset
    @echo "preset: {{p}}"

configure *args="":
    cmake --preset {{preset}} {{args}}

build *args="":
    cmake --build --preset {{preset}} --parallel

compile *args="": (configure args) build

rebuild:
    just clean
    just compile


linux:
    just preset=linux-debug compile

linux-rel:
    just preset=linux-release compile

arm-generic-nolibc:
    just preset=arm-none-eabi-nolibc-generic compile

arm-generic-picolibc:
    just preset=arm-none-eabi-picolibc-generic compile

arm-qemu-picolibc:
    just preset=qemu-mps3-an524-picolibc-debug compile

run *args="":
    build/{{preset}}/tpl {{args}}

test:
    ctest --test-dir build/{{preset}} --output-on-failure

test-sh:
    ./tests/run.sh

check:
    ./tests/run_valgrind.sh

leaks:
    ./tests/run_valgrind_leaks.sh

format:
    cmake --build --preset {{preset}} --target format

format-check:
    cmake --build --preset {{preset}} --target format-check

lint:
    cmake --build --preset {{preset}} --target lint

cache:
    cmake -LA -N build/{{preset}} | sort

vars:
    cmake -LH -N build/{{preset}}

flags:
    cmake --build --preset {{preset}} --parallel -- VERBOSE=1

clean:
    rm -rf build/{{preset}}

clean-all:
    rm -rf build

gen-compat-stubs preset="arm-none-eabi-nolibc-generic":
    rm -rf build/{{preset}}
    cmake --preset {{preset}} -DCOMPILE_DIAGNOSTICS_FORMAT=text
    ./tools/gen-compat-stubs cmake --build --preset {{preset}} -- -k -j$(nproc)

report preset="arm-none-eabi-nolibc-generic":
    rm -rf build/{{preset}}
    cmake --preset {{preset}} -DCOMPILE_DIAGNOSTICS_FORMAT=json-file
    cmake --build --preset {{preset}} -- -k -j1$(nproc) || true
    ./tools/create-report build/{{preset}}

report-preview preset="arm-none-eabi-nolibc-generic":
    rm -rf build/{{preset}}
    cmake --preset {{preset}} -DCOMPILE_DIAGNOSTICS_FORMAT=json-file
    cmake --build --preset {{preset}} -- -k -j1$(nproc) || true
    ./tools/create-report build/{{preset}} --preview

# TEMPORARY (both of these need to be reworked into sth more flexible / universal)

qemu:
    qemu-system-arm \
      -M mps3-an524 \
      -chardev stdio,signal=on,mux=on,id=stdio0 \
      -serial none \
      -monitor none \
      -semihosting-config enable=on,target=native,chardev=stdio0 \
      -nographic \
      -kernel build/qemu-mps3-an524-picolibc-debug/tpl.elf \
      -gdb tcp::1234

gdb:
    # May need to use arm-none-eabi-gdb depending on os
    gdb build/qemu-mps3-an524-picolibc-debug/tpl.elf
