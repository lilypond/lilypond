/*
  voice-gravs.hh -- declare Voice_engravers

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef VOICEGRAVS_HH
#define VOICEGRAVS_HH

#include "engraver-group.hh"
#include "interpreter.hh"

class Voice_engravers : public Interpreter, public Engraver_group_engraver {
public:
    NAME_MEMBERS();

protected:
    virtual bool interpret_request_b(Request*);
    virtual Interpreter* interpreter_l() { return this; }
};


#endif // VOICEGRAVS_HH
