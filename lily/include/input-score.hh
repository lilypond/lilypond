/*
  input-score.hh -- declare 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef INPUTSCORE_HH
#define INPUTSCORE_HH

#include "varray.hh"
#include "proto.hh"
#include "plist.hh"
#include "string.hh"
#include "input.hh"

/// the total music def of one movement
class Input_score : public Input {
public:
    int errorlevel_i_;
    
    /// paper_, staffs_ and commands_ form the problem definition.
    Paper_def *paper_p_;
    Midi_def* midi_p_;
    IPointerList<Input_staff*> staffs_;

    
    /* *************************************************************** */
    Input_score();
    Input_score(Input_score const&);

    void add(Input_staff*);
    ~Input_score();
    /// construction
    void set(Paper_def* paper_p);
    void set(Midi_def* midi_p);
    void print() const;
    Score*parse();
};

#endif
