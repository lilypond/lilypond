/*
  vertical-align-spanner.hh -- declare Vertical_align_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef VERTICAL_ALIGN_SPANNER_HH
#define VERTICAL_ALIGN_SPANNER_HH

#include "spanner.hh"
#include "vertical-align-elem.hh"

class Vertical_align_spanner : public Vertical_align_element, public Spanner
{
public:
    DECLARE_MY_RUNTIME_TYPEINFO;
    SCORE_ELEM_CLONE(Vertical_align_spanner);
    virtual void do_print() const { Vertical_align_element::do_print () ; }
    
};
#endif // VERTICAL_ALIGN_SPANNER_HH
