/*
  input-staff.hh -- declare Input_staff

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef INPUTSTAFF_HH
#define INPUTSTAFF_HH

#include "string.hh"
#include "plist.hh"
#include "varray.hh"
#include "proto.hh"

struct Input_staff {
    
    char const * defined_ch_c_l_;
    String type;
    
    IPointerList<Input_music*> music_;
    
    /* *************** */
    ~Input_staff();
    void add(Input_music*m);
    Input_staff(Input_staff const&);
    Input_staff(String);
    Staff* parse(Score*);
    void print() const;
};


#endif // INPUTSTAFF_HH

