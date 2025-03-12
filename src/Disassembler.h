#ifndef __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_DISASSEMBLER_H_
#define __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_DISASSEMBLER_H_

#include "IFoundInstructionNotifier.h"
#include "SourceBinary.h"

class Disassembler {
public:
  Disassembler(SourceBinary& src,
               IFoundInstructionNotifier *n)
      : src_(src),  n_(n) {}
    
  void disassemble();

private:
  IFoundInstructionNotifier *n_;
  SourceBinary& src_;
};

#endif // __CLIONPROJECTS_M68K_DISASSEMBLER_SRC_DISASSEMBLER_H_