/*
  inputstaff.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef INPUTSTAFF_HH
#define INPUTSTAFF_HH

#include "string.hh"
#include "plist.hh"
#include "varray.hh"
#include "proto.hh"

struct Input_staff {
    
    /// defined where?    
    const char * defined_ch_c_l_;
    String type;
    IPointerList<Input_command*> commands_;
    IPointerList<Input_music*> music_;

    /* *************** */

    void add(Input_music*m);
    Input_staff(Input_staff const&);
    Input_staff(String);
    void add(Array<Input_command*> &s);
    Staff* parse(Score*);
    void print() const;
};


#endif // INPUTSTAFF_HH

