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
public:
  Stem * stem_l_;
  int balltype_i_;
  int position_i_;

  Dots * dots_l_;

  void add_dots (Dots *);
  Rhythmic_head ();

  int dots_i ()const;
  virtual int position_i () const;
protected:
  virtual void do_post_processing ();
  virtual void do_pre_processing ();
  virtual void do_print () const;
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
};

#endif // RHYTHMIC_HEAD_HH
