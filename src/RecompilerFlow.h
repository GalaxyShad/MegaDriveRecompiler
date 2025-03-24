#ifndef __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERFLOW_H_
#define __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERFLOW_H_

#include "RoutineContext.h"
#include "SourceBinary.h"
#include <format>
#include <map>
#include <stack>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

class RecompilerFlow {
public:
    RecompilerFlow(SourceBinary &source)
        : src_(source), routine_(source.get_pc()) {
        add_routine_and_jmp(source.get_pc());
    }

    RoutineContext &ctx() { return program_[routine_]; }

    const std::map<u32, RoutineContext> &program() { return program_; }

    void add_known_labels(std::unordered_map<u32, std::string> &known_labels) {
        known_labels_ = &known_labels;
    }

    void add_known_xn_values(std::unordered_map<u32, std::vector<i32>>& known_xn_) {
        known_xn_values_ = &known_xn_;
    }

    std::string get_name_for_label(u32 adr) {
        if (known_labels_ != nullptr && known_labels_->contains(adr)) {
            return (*known_labels_)[adr];
        }

        return std::format("loc_{:X}", adr);
    }

    void add_routine(u32 adr) {
        if (program_.contains(adr)) {
            throw std::invalid_argument("routine already exists");
        };

        program_[adr] = {
            .adr = adr,
            .last_pc = src_.get_pc(),
            .name = get_name_for_label(adr),
        };
    }

    void ret() {
        stack_.pop();
        routine_ = stack_.top()->adr;
        src_.set_pc(program_[routine_].last_pc);
    }

    void jmp(u32 adr) {
        ctx().last_pc = src_.get_pc();
        stack_.push(&program_[adr]);
        routine_ = adr;
        src_.set_pc(adr);
    }

    void jmp_xn(u32 pc, u32 base_adr) {
        auto& xn_list = get_xn_list_for_adr(pc);

        u32 dst_adr = base_adr + xn_list[xn_jmp_index_];

        if (xn_jmp_index_ != xn_list.size()-1) {
            ctx().last_pc = pc;
            xn_jmp_index_++;
        } else {
            ctx().last_pc = src_.get_pc();
            xn_jmp_index_ = 0;
        }

        stack_.push(&program_[dst_adr]);
        routine_ = dst_adr;
        src_.set_pc(dst_adr);    
    }

    const std::vector<i32>& get_xn_list_for_adr(u32 adr) {
        if (known_xn_values_ == nullptr) {
            throw std::invalid_argument("no known_xn_values_ set");
        }

        if (!known_xn_values_->contains(adr) || known_xn_values_->at(adr).empty()) {
            throw std::invalid_argument(std::format("Xn jump values for adr <{:X}> not specified or empty in provided config", adr));
        }

        return known_xn_values_->at(adr);
    }

private:
    void add_routine_and_jmp(u32 adr) {
        add_routine(adr);
        jmp(adr);
    }

private:
    std::map<u32, RoutineContext> program_;
    std::stack<RoutineContext *> stack_;
    u32 routine_;

    SourceBinary &src_;

    std::unordered_map<u32, std::string> *known_labels_ = nullptr;
    std::unordered_map<u32, std::vector<i32>> *known_xn_values_ = nullptr;
    i32 xn_jmp_index_ = 0;
};

#endif // __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERFLOW_H_