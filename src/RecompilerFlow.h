#ifndef __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERFLOW_H_
#define __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERFLOW_H_

#include "RoutineContext.h"
#include "SourceBinary.h"
#include <format>
#include <map>
#include <ranges>
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

    RecompilerFlow(SourceBinary &source, std::unordered_map<u32, std::string> &known_labels)
        : src_(source), known_labels_(&known_labels), routine_(source.get_pc()) {
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
            .jumped_count = 0,
            .name = get_name_for_label(adr),
        };
    }

    void ret() {
        stack_.pop();
        if (stack_.empty()) {
            throw std::invalid_argument("stack empty");
        }

        routine_ = stack_.top()->adr;
        if (!program_[routine_].addresses_to_jmp.empty()) {
            jmp(program_[routine_].addresses_to_jmp.top());
            program_[routine_].addresses_to_jmp.pop();
            return;
        }

        if (program_[routine_].is_translation_finished) {
            ret();
            return;
        }
        src_.set_pc(program_[routine_].last_pc);
    }

    void jmp(u32 adr, bool exit_on_return = false) {
        program_.at(adr).jumped_count++;
        ctx().last_pc = src_.get_pc();
        ctx().is_translation_finished = exit_on_return;
        stack_.push(&program_.at(adr));
        routine_ = adr;
        src_.set_pc(adr);
    }

    void jmp_multiple(std::vector<u32> addresses, bool exit_on_return = false) {
        if (addresses.empty())
            return;
        
        for (auto a : std::ranges::reverse_view(addresses)) {
            if (!program_.contains(a)) {
                add_routine(a);
                program_.at(a).last_pc = a;
                stack_.push(&program_.at(a));
            }
        }
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

    void add_stack(u32 adr){ 
        stack_.push(&program_.at(adr));
    }
    void set_routine(u32 adr){ 
        routine_ = adr;
    }
    void set_program(u32 adr){
        program_.at(adr).last_pc = adr;
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