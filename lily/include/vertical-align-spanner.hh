/*
  vertical-align-spanner.hh -- declare Vertical_align_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef VERTICAL_ALIGN_SPANNER_HH
#define VERTICAL_ALIGN_SPANNER_HH

#include "spanner.hh"
#include "align-element.hh"

class Vertical_align_spanner : public Align_element, public Spanner
{
public:
  
  VIRTUAL_COPY_CONS(Score_element);
  Vertical_align_spanner ();
  virtual void do_print() const ;
    
};
#endif // VERTICAL_ALIGN_SPANNER_HH
