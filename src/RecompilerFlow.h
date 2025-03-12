#ifndef __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERFLOW_H_
#define __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERFLOW_H_

#include "RoutineContext.h"
#include "SourceBinary.h"
#include <format>
#include <map>
#include <stack>
#include <string>
#include <unordered_map>

class RecompilerFlow {
public:
  RecompilerFlow(SourceBinary& source) : src_(source), routine_(source.get_pc()) {
    add_routine_and_jmp(source.get_pc());
  }

  RoutineContext& ctx() { return program_[routine_]; }

  const std::map<u32, RoutineContext>& program() { return program_; }

  void add_known_labels(std::unordered_map<u32, std::string>& known_labels) {
    known_labels_ = &known_labels;
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
    stack_.push(&program_[adr]);
    routine_ = adr;
    src_.set_pc(adr);
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

  SourceBinary& src_;

  std::unordered_map<u32, std::string>* known_labels_ = nullptr;
};

#endif // __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_RECOMPILERFLOW_H_