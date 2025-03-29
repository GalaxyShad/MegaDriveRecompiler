#include <print>
#include <stdexcept>
#include <string>

#include "Disassembler.h"
#include "Recompiler.h"
#include "RecompilerFlow.h"
#include "RecompilerSourceGenerator.h"
#include "SourceBinary.h"
#include "tinyint.h"

const u32 game_init_adr = 0x364;

auto main() -> int {

    SourceBinary binary("./build/s1built.bin");

    std::unordered_map<u32, std::vector<i32>> known_xn_values = {
        {0x390, {0x00, 0x04, 0x08, 0x0C, 0x10, 0x14, 0x18, 0x1C}},
        {0x14C2, {0x14FA, 0x1504}},
        {0x175C, {0, 2, 4, 6, 8, 0xA, 0xC, 0xE}}
    };

    binary.set_pc(game_init_adr);

    RecompilerFlow flow(binary);

    flow.add_known_xn_values(known_xn_values);

    Recompiler recomp(binary, flow);
    Disassembler disasm(binary, &recomp);

    while (1) {
        try {
            disasm.disassemble();
        } catch (std::invalid_argument e) {
            recomp.write_all_to_file();
            std::println("[ERR] at {:X}, last_value: {:X} -> {}",
                         binary.get_pc(), binary.last_read_value(), e.what());
            return 1;
        }
    }
}