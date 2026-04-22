set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SYSTEM_PROCESSOR ARM)

set(TOOLCHAIN_PREFIX arm-none-eabi)

set(CMAKE_C_COMPILER ${TOOLCHAIN_PREFIX}-gcc)
set(CMAKE_ASM_COMPILER ${TOOLCHAIN_PREFIX}-gcc)
set(CMAKE_AR ${TOOLCHAIN_PREFIX}-ar)
set(CMAKE_RANLIB ${TOOLCHAIN_PREFIX}-ranlib)
set(CMAKE_OBJCOPY ${TOOLCHAIN_PREFIX}-objcopy)
set(CMAKE_SIZE ${TOOLCHAIN_PREFIX}-size)

set(CMAKE_EXECUTABLE_SUFFIX ".elf")
set(CMAKE_EXECUTABLE_SUFFIX_C ".elf")
set(CMAKE_EXECUTABLE_SUFFIX_CXX ".elf")
set(CMAKE_EXECUTABLE_SUFFIX_ASM ".elf")

set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
set(CMAKE_TRY_COMPILE_PLATFORM_VARIABLES TPL_BAREMETAL_CPU TPL_BAREMETAL_FPU
                                         TPL_BAREMETAL_FLOAT_ABI)

set(TPL_BAREMETAL_CPU
    ""
    CACHE STRING "CPU (e.g. cortex-m0/m3/m4/m7)")
set(TPL_BAREMETAL_FPU
    ""
    CACHE STRING "FPU (e.g. fpv4-sp-d16) or empty")
set(TPL_BAREMETAL_FLOAT_ABI
    ""
    CACHE STRING "Float ABI (soft/softfp/hard) or empty")
set(TPL_LINKER_SCRIPT
    ""
    CACHE FILEPATH "Linker script (.ld)")

# No standard C library — compat stubs will be used for required headers
set(TPL_LIBC_BACKEND
    "none"
    CACHE STRING "" FORCE)

if(TPL_BAREMETAL_CPU STREQUAL "")
  message(FATAL_ERROR "TPL_BAREMETAL_CPU must be set for arm-none-eabi-nolibc")
endif()

set(_arch_flags "-mcpu=${TPL_BAREMETAL_CPU} -mthumb")
if(TPL_BAREMETAL_FPU AND TPL_BAREMETAL_FLOAT_ABI)
  string(APPEND _arch_flags
         " -mfpu=${TPL_BAREMETAL_FPU} -mfloat-abi=${TPL_BAREMETAL_FLOAT_ABI}")
endif()

set(CMAKE_C_FLAGS_INIT "${_arch_flags} -ffreestanding")
set(CMAKE_ASM_FLAGS_INIT "${_arch_flags}")
set(CMAKE_EXE_LINKER_FLAGS_INIT "--specs=nosys.specs -nostartfiles -nostdlib")

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)
