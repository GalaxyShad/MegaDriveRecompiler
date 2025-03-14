#ifndef __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERSOURCEGENERATOR_H_
#define __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERSOURCEGENERATOR_H_

#include "RecompilerFlow.h"
#include <fstream>

class RecompilerSourceGenerator {
public:
    RecompilerSourceGenerator(RecompilerFlow& flow) : flow_(flow) {}

    void write_to_file() {
        std::ofstream out("recompiled/recompiled.c");

        out << "#include \"context.h\"\n\n";

        for (const auto &pair : flow_.program()) {
        out << "void " << pair.second.name << "(Context* ctx);\n";
        }

        out << "\n/* -------------------------------------- */\n";
        out << "\n/* -------------------------------------- */\n\n";

        for (const auto &pair : flow_.program()) {

        out << "void " << pair.second.name << "(Context* ctx) {\n";
        for (const auto &line : pair.second.line_list) {
            out << "    " << line << "\n";
        }
        out << "}\n\n";
        }

        out.close();
    }
private:
    RecompilerFlow& flow_;
};

#endif // __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERSOURCEGENERATOR_H_