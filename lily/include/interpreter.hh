/*
  interpretor.hh -- declare Interpreter

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef Interpreter_HH
#define Interpreter_HH

class Interpreter {
public:
    virtual bool interpret_request_b(Request*) { return false;}
};

#endif // Interpreter_HH
