/*
  bar.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef BAR_HH
#define BAR_HH
#include "item.hh"

/**
  A vertical bar.
 */
class Bar:public Item
{
public:
  VIRTUAL_COPY_CONS(Score_element);
  Bar(SCM);

  Molecule compound_barline (String, Real height) const;
  Molecule simple_barline (Real wid, Real height) const;      
  static SCM brew_molecule (SCM);
  

public:
  SCM member_before_line_breaking ();
  static SCM before_line_breaking (SCM);

  virtual Real get_bar_size () const;
};
#endif // BAR_HH

