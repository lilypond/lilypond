/*
  rhythmic-head.hh -- declare Rhythmic_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef RHYTHMIC_HEAD_HH
#define RHYTHMIC_HEAD_HH

#include "item.hh"
#include "staff-symbol-referencer.hh"

class Rhythmic_head : public Item, public Staff_symbol_referencer
{
  Dots * dots_l_;
  Stem * stem_l_;
public:

  int balltype_i_;

  void add_dots (Dots *);
  Rhythmic_head ();
  Stem * stem_l ()const;
  Dots * dots_l ()const;
  int dots_i ()const;
protected:
  virtual void do_post_processing ();

  virtual void do_print () const;
};

#endif // RHYTHMIC_HEAD_HH
