/*
  rhythmic-head.hh -- declare 

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef RHYTHMIC_HEAD_HH
#define RHYTHMIC_HEAD_HH

#include "item.hh"

class Rhythmic_head : public Item
{
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
   
  int balltype_i_;
  int dots_i_;
  Dots * dots_l_;

  void add (Dots *);
  Rhythmic_head ();
protected:
  virtual void do_add_processing ();
  virtual void do_print () const;
  virtual void do_substitute_dependent (Score_elem*,Score_elem*);
};

#endif // RHYTHMIC_HEAD_HH
