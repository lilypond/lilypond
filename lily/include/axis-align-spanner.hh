/*
  vertical-align-spanner.hh -- declare Vertical_align_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef VERTICAL_ALIGN_SPANNER_HH
#define VERTICAL_ALIGN_SPANNER_HH

#include "spanner.hh"
#include "align-element.hh"
#include "axis-group-spanner.hh"

class Axis_align_spanner : public virtual Align_element,
			   public virtual Axis_group_spanner
{
public:
  VIRTUAL_COPY_CONS(Score_element);
  Axis_align_spanner ();
  virtual void do_print() const {}
};
#endif // VERTICAL_ALIGN_SPANNER_HH
