/*
  text-def.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef TEXT_DEF_HH
#define TEXT_DEF_HH

#include "general-script-def.hh"
#include "string.hh"
#include "lily-proto.hh"
#include "input.hh"

class Text_def : public General_script_def {
protected:
// 	huh? move to public
//    virtual Atom get_atom (Paper_def* p, Direction dir_) const;
    DECLARE_MY_RUNTIME_TYPEINFO;
    VIRTUAL_COPY_CONS(Text_def,General_script_def);
public:
    /**
      centered , or aligned?

      -1 = raggedright, 0 = center, 1 = raggedleft
     */
    Direction align_i_;

    String text_str_;
    String style_str_;
    
    virtual void do_print() const;
    virtual Atom get_atom (Paper_def* p, Direction dir_) const;
    virtual ~Text_def() {};
    virtual bool do_equal_b (const General_script_def*) const;
    Text_def();
    virtual void print() const;
    Interval width (Paper_def*) const;
};

#endif // TEXT_DEF_HH

