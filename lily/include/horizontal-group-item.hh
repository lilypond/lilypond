/*
  horizontal-group-item.hh -- declare Horizontal_group_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef HORIZONTAL_GROUP_ITEM_HH
#define HORIZONTAL_GROUP_ITEM_HH

#include "horizontal-group-elem.hh"
#include "axis-group-item.hh"

/**
  Group stuff in horizontal sense. Example: Paper_column
 */
class Horizontal_group_item : public Axis_group_item, public Horizontal_group_element {
protected:
  virtual void remove_all() { Horizontal_group_element::remove_all (); }
  virtual void do_unlink () { 
    Axis_group_item::do_unlink ();
  }
  virtual void do_junk_links() {
    Axis_group_item::do_junk_links();
  }
  virtual void do_print() const;
public:
  virtual void add_element (Graphical_element*e) { Horizontal_group_element::add_element (e); }
  virtual void remove_element (Graphical_element*e) { Horizontal_group_element::remove_element (e); }
  DECLARE_MY_RUNTIME_TYPEINFO;
  SCORE_ELEM_CLONE(Horizontal_group_item);
};

#endif // HORIZONTAL_GROUP_ITEM_HH
