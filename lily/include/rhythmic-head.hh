/*
  rhythmic-head.hh -- declare Rhythmic_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef RHYTHMIC_HEAD_HH
#define RHYTHMIC_HEAD_HH

#include "item.hh"

class Rhythmic_head : public Item
{
public:
  
   
  int balltype_i_;
  int dots_i_;
  Dots * dots_l_;

  void add_dots (Dots *);
  Rhythmic_head ();
protected:
  virtual void do_add_processing ();
  virtual void do_print () const;
  virtual void do_substitute_dependent (Score_element*,Score_element*);
};

#endif // RHYTHMIC_HEAD_HH
