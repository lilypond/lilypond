/*
  text-def.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef TEXTDEF_HH
#define TEXTDEF_HH

#include "string.hh"
#include "proto.hh"

struct Text_def  {
    int align_i_;
    String text_str_;
    String style_str_;
    char const* defined_ch_C_;
  

    /* *************** */
    virtual ~Text_def() {};
    bool compare(const Text_def&);
    Text_def();
    virtual void print() const;
    virtual Atom create_atom(Paper_def*) const;
};

#endif // TEXTDEF_HH

