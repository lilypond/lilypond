/*
  horizontal-vertical-group-item.hh -- declare Horizontal_vertical_group_item
  
  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  virtual void do_unlink () { Horizontal_vertical_group_element::do_unlink (); }
  SCORE_ELEMENT_CLONE(Horizontal_vertical_group_item);
public:
  Horizontal_vertical_group_item ();

  DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif // HORIZONTAL_VERTICAL_GROUP_ITEM_HH
