/*
  text-def.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef TEXT_DEF_HH
#define TEXT_DEF_HH

#include "string.hh"
#include "lily-proto.hh"
#include "input.hh"

class Text_def : public Input {
public:
    /**
      centered , or aligned?

      -1 = raggedright, 0 = center, 1 = raggedleft
     */
    int align_i_;
    Paper_def* pdef_l_;
    String text_str_;
    String style_str_;
    
    /* *************** */
    virtual ~Text_def() {};
    bool compare(const Text_def&);
    Text_def();
    virtual void print() const;
    Atom create_atom() const;
    Interval width() const;
};

#endif // TEXT_DEF_HH

