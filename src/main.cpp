#include <print>
#include <stdexcept>
#include <string>

#include "Disassembler.h"
#include "Recompiler.h"
#include "RecompilerFlow.h"
#include "SourceBinary.h"
#include "tinyint.h"

const u32 game_init_adr = 0x364;

auto main() -> int {

    SourceBinary binary("./build/s1built.bin");

    binary.set_pc(game_init_adr);

    Recompiler recomp(binary);
    Disassembler disasm(binary, &recomp);

    while (1) {
        try {
            disasm.disassemble();
        } catch (std::invalid_argument e) {
            std::println("[ERR] at {:X}, last_value: {:X} -> {}",
                         binary.get_pc(), binary.last_read_value(), e.what());
            return 1;
        }
    }
}