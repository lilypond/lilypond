/*
  horizontal-group-item.hh -- declare Horizontal_group_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef HORIZONTAL_GROUP_ITEM_HH
#define HORIZONTAL_GROUP_ITEM_HH

#include "horizontal-group-element.hh"
#include "axis-group-item.hh"

/**
  Group stuff in horizontal sense. Example: Paper_column
 */
class Horizontal_group_item : public Axis_group_item, public Horizontal_group_element {
protected:
  virtual void do_unlink () { 
    Axis_group_item::do_unlink ();
  }
  virtual void do_junk_links() {
    Axis_group_item::do_junk_links();
  }
  virtual void do_print() const;
public:
  Horizontal_group_item ();
  DECLARE_MY_RUNTIME_TYPEINFO;
  SCORE_ELEMENT_CLONE(Horizontal_group_item);
};

#endif // HORIZONTAL_GROUP_ITEM_HH
