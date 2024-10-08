cmake_minimum_required(VERSION 3.16.3)

project(Micro_C_Compiler)

# Find fmt
find_package(fmt REQUIRED)

message(STATUS "Found fmt ${fmt_VERSION}")
message(STATUS "Using fmt in ${fmt_DIR}")

# Find LLVM
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in ${LLVM_DIR}")

if (NOT ${LLVM_ENABLE_ASSERTIONS})
    message(WARNING "Using an LLVM build without assertions")
endif()

llvm_map_components_to_libnames(LLVM_LIBRARIES
    core
)

# list of all targets that need to be built
set(MICROCC_ALL_TARGETS lexer ast parser sema microcc)

function(add_microcc_library name)
    if ("${name}" IN_LIST MICROCC_ALL_TARGETS)
        add_library(${ARGV})
    else()
        message(STATUS "Using pre-built library '${name}' instead of building from source")
    endif()
endfunction()

# lexer
add_microcc_library(lexer
    src/lexer/lexer.cpp
    src/lexer/token.cpp
    )

# ast
add_microcc_library(ast
    src/ast/prettyprinter.cpp
    )

# parser
add_microcc_library(parser
    src/parser/parser.cpp
    )

# sema
add_microcc_library(sema
    src/sema/collectfuncdeclspass.cpp
    src/sema/scoperesolutionpass.cpp
    src/sema/typecheckingpass.cpp
    src/sema/util.cpp
    )

# driver
add_executable(microcc
    src/driver/main.cpp
    )

target_link_libraries(microcc PUBLIC lexer ast parser sema)

# set properties common to all targets
foreach(TARGET ${MICROCC_ALL_TARGETS})
    target_include_directories(${TARGET} PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/src")
    target_include_directories(${TARGET} PRIVATE "${LLVM_INCLUDE_DIRS}")
    target_link_libraries(${TARGET} PRIVATE "${LLVM_LIBRARIES}")
    target_link_libraries(${TARGET} PRIVATE fmt::fmt)

    if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/lib")
        target_link_directories(${TARGET} PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/lib")
    endif()

    if (NOT ${LLVM_ENABLE_RTTI})
        target_compile_options(${TARGET} PRIVATE "-fno-rtti")
        target_link_options(${TARGET} PRIVATE "-fno-rtti")
    endif()
endforeach()

# testing
find_program(LIT NAMES llvm-lit lit lit.py)
find_program(FILECHECK NAMES FileCheck)
find_program(LLVM_NOT NAMES llvm-not not)

if (LIT AND FILECHECK AND LLVM_NOT)
    if (DEFINED DODONA_RESOURCES_PATH)
        set(TEST_SOURCE_ROOT "${DODONA_RESOURCES_PATH}/")
        configure_file(${DODONA_RESOURCES_PATH}/lit.cfg.in test/lit.cfg)
    else()
        set(TEST_SOURCE_ROOT "${CMAKE_SOURCE_DIR}/test/")
        configure_file(test/lit.cfg.in test/lit.cfg)
    endif()

    add_custom_target(check
        COMMAND ${LIT} -sv ${CMAKE_BINARY_DIR}/test -o results.json $ENV{LIT_EXTRA_ARGS}
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/test
        USES_TERMINAL
    )

    add_dependencies(check microcc)
else()
    message(WARNING "'check' target disabled: need lit, FileCheck, and not.")
endif()

