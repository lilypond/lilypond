/*
  axis-item.hh -- declare Axis_group_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef AXIS_ITEM_HH
#define AXIS_ITEM_HH

#include "item.hh"

class Axis_group_item :  public virtual Item
{
public:
  Axis_group_item ();
  VIRTUAL_COPY_CONS(Score_element);
};

#endif // AXIS_ITEM_HH
