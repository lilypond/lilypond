/*
  horizontal-vertical-group-item.hh -- declare Horizontal_vertical_group_item
  
  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef HORIZONTAL_VERTICAL_GROUP_ITEM_HH
#define HORIZONTAL_VERTICAL_GROUP_ITEM_HH

#include "axis-group-item.hh"
#include "horizontal-vertical-group-element.hh"


/**
  Treat a collection of items as a unity
 */
class Horizontal_vertical_group_item  : public Axis_group_item, public Horizontal_vertical_group_element {
protected:
  virtual void do_print() const;
  VIRTUAL_COPY_CONS(Score_element);
public:
  Horizontal_vertical_group_item ();

  
};

#endif // HORIZONTAL_VERTICAL_GROUP_ITEM_HH
