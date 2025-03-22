#ifndef __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_SOURCEBINARY_H_
#define __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_SOURCEBINARY_H_

#include "enums.h"
#include "tinyint.h"
#include <format>
#include <fstream>
#include <stdexcept>

class SourceBinary {
public:
    SourceBinary(std::string path) {
        file_.open(path, std::ios::binary);

        if (!file_.is_open()) {
            throw std::invalid_argument(
                std::format("Cannot open file {}", path));
        }
    }

    ~SourceBinary() { file_.close(); }

    u32 get_next_by_size(Size s) {
        switch (s) {
        case Size::Byte:
            return get_next_byte();
        case Size::Word:
            return get_next_word();
        case Size::Long:
            return get_next_long();
        }
    }

    u8 get_next_byte() {
        file_.seekg(pc_);
        u8 value = file_.get();
        pc_ += 1;
        last_read_value_ = value;
        return value;
    }

    u16 get_next_word() {
        file_.seekg(pc_);
        u8 low_byte = file_.get();
        u8 high_byte = file_.get();
        pc_ += 2;

        last_read_value_ = (((u16)low_byte) << 8) | high_byte;

        return last_read_value_;
    }

    u32 get_next_long() {
        file_.seekg(pc_);
        u8 b[4];
        for (int i = 0; i < 4; i++) {
            b[i] = file_.get();
        }
        pc_ += 4;

        last_read_value_ = (((u32)b[0]) << 24) | (((u32)b[1]) << 16) |
                           (((u32)b[2]) << 8) | (((u32)b[3]) << 0);

        return last_read_value_;
    }

    u32 get_pc() { return pc_; }
    void set_pc(u32 pc) { pc_ = pc; }

    u32 last_read_value() { return last_read_value_; }

private:
    std::ifstream file_;
    u32 pc_;
    u32 last_read_value_;
};

#endif // __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_SOURCEBINARY_H_