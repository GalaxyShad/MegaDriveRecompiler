#ifndef __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_ROUTINECONTEXT_H_
#define __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_ROUTINECONTEXT_H_

#include "tinyint.h"
#include <string>
#include <vector>

struct RoutineContext {
    u32 adr;
    std::string name;

    u32 last_pc;

    std::vector<std::string> line_list;

    void writeln(const std::string &line) { line_list.push_back(line); }
};

#endif // __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_ROUTINECONTEXT_H_