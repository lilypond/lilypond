/*
  axis-group-spanner.hh -- declare Axis_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef AXIS_GROUP_SPANNER_HH
#define AXIS_GROUP_SPANNER_HH

#include "spanner.hh"
#include "axis-group-element.hh"

/** An element which groups a line in a certain direction. The most
  useful example of this is the Vertical_group_spanner */
class Axis_group_spanner : public virtual Axis_group_element,
			   public virtual Spanner

{
public:
  VIRTUAL_COPY_CONS(Score_element);
};

#endif // SPAN_AXIS_GROUP_HH
