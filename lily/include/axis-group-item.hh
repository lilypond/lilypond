/*
  axis-item.hh -- declare Axis_group_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef AXIS_ITEM_HH
#define AXIS_ITEM_HH

#include "axis-group-element.hh" 
#include "item.hh"

/**
  A grouping item. Its special support is in do_breakable_col_processing().
 */

class Axis_group_item : public virtual Axis_group_element, public Item {
protected:
  virtual void do_breakable_col_processing();
  void OK() const;
  virtual void do_print() const;
  virtual void do_junk_links() { 
    Item::do_junk_links();
    Axis_group_element::do_junk_links();
  }
  virtual void do_unlink() {
    Item::do_unlink();
    Axis_group_element::do_unlink();
  }
public:
  
};

#endif // AXIS_ITEM_HH
