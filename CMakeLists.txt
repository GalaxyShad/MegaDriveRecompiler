cmake_minimum_required(VERSION 3.10)

project(m68k_disasm)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_executable(m68k_disasm_exe src/main.cpp src/Recompiler.cpp src/Disassembler.cpp src/RecompilerConfig.cpp)

target_include_directories(m68k_disasm_exe PRIVATE "./src")