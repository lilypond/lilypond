/*
  interpretor.hh -- declare 

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef Interpreter_HH
#define Interpreter_HH

class Interpreter {
public:
    /// link to my definition
    Input_register * ireg_l_;
    void interpret_request(Request *);
    NAME_MEMBERS();
    ~Interpreter();
    /** typeset any items/spanners. Default: do nothing
     */
    virtual void do_pre_move_processing(){}
};

class Paper_interpreter : Interpreter {
    
};

class Midi_interpreter : Interpreter {
};

#endif // Interpreter_HH
