#include <print>
#include <stdexcept>
#include <string>

#include "Config.h"
#include "Disassembler.h"
#include "Recompiler.h"
#include "RecompilerFlow.h"
#include "RecompilerSourceGenerator.h"
#include "SourceBinary.h"
#include "tinyint.h"

RecompilerConfig load_config();

auto main() -> int {

    auto cfg = load_config();

    SourceBinary binary(cfg.binary_name);

    binary.set_pc(cfg.start_adr);

    RecompilerFlow flow(binary, cfg.known_labels);

    flow.add_known_xn_values(cfg.known_xn_values);

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