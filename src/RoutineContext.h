#ifndef __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_ROUTINECONTEXT_H_
#define __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_ROUTINECONTEXT_H_

#include "tinyint.h"
#include <stack>
#include <string>
#include <vector>

struct RoutineContext {
    u32 adr;
    std::string name;

    u32 last_pc;
    bool is_translation_finished;

    // std::stack<u32> addresses_to_jmp;
    std::vector<std::string> line_list;

    void writeln(const std::string &line) { line_list.push_back(line); }
};

#endif // __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_ROUTINECONTEXT_H_