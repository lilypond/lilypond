/*
  text-def.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef TEXT_DEF_HH
#define TEXT_DEF_HH

#include "general-script-def.hh"
#include "string.hh"
#include "lily-proto.hh"
#include "input.hh"

class Text_def : public General_script_def 
{
public:
  VIRTUAL_COPY_CONS(General_script_def);

  /**
     Alignment of typeset text wrt center.

     LEFT = raggedright, CENTER = centered, RIGHT = raggedleft
  */
  Direction align_dir_;

  String text_str_;
  String style_str_;
    
  virtual void do_print() const;
  virtual Direction staff_dir() const;
  virtual Molecule get_molecule (Paper_def* p, Direction dir_) const;
  virtual ~Text_def() {};
  virtual bool do_equal_b (const General_script_def*) const;
  Text_def();
  virtual void print() const;
};

#endif // TEXT_DEF_HH

