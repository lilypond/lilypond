/*
  interpretor.hh -- declare Interpreter

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef INTERPRETER_HH
#define INTERPRETER_HH

#include "lily-proto.hh"

class Interpreter {
public:
    int music_list_i_;
    Interpreter();
    virtual ~Interpreter();
    virtual bool interpret_request_b(Request*) { return false;}
};

#endif // INTERPRETER_HH
