/*
  voice-regs.hh -- declare Voice_registers

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef VOICEREGS_HH
#define VOICEREGS_HH

#include "register-group.hh"
#include "interpreter.hh"

class Voice_registers : public Interpreter, public Register_group_register {
public:
    Voice_registers();
    NAME_MEMBERS();

protected:
    virtual bool interpret_request_b(Request*);
    virtual Interpreter* interpreter_l() { return this; }
    virtual void do_print() const;
};


#endif // VOICEREGS_HH
