/*
  axis-item.hh -- declare Axis_group_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef AXIS_ITEM_HH
#define AXIS_ITEM_HH

#include "axis-group-element.hh" 
#include "item.hh"

/**
  A grouping item. Its special support is in do_breakable_col_processing().
 */

class Axis_group_item : public virtual Axis_group_element,
			public virtual Item {
protected:
  virtual void do_breakable_col_processing();
  void OK() const;
  virtual void do_print() const;
public:
  VIRTUAL_COPY_CONS(Score_element);
};

#endif // AXIS_ITEM_HH
