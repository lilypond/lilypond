/*
  inputscore.hh -- declare 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef INPUTSCORE_HH
#define INPUTSCORE_HH

#include "varray.hh"
#include "proto.hh"
#include "plist.hh"
#include "string.hh"


/// the total music def of one movement
struct Input_score {
    /// defined where?    
    const char* defined_ch_c_l_;
    int errorlevel_i_;
    
    /// paper_, staffs_ and commands_ form the problem definition.
    Paperdef *paper_p_;
    Mididef* midi_p_;
    IPointerList<Input_staff*> staffs_;

    Input_music * score_wide_music_p_;
    
    /* *************************************************************** */
    Input_score();
    Input_score(Input_score const&);

    void add(Input_staff*);
    ~Input_score();
    /// construction
    void set(Paperdef*);
    void set(Mididef* midi_p);
    void print() const;
    Score*parse();
    void set(Input_music*);
};

#endif
