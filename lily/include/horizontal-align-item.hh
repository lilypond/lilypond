/*
  horizontal-align-item.hh -- declare Horizontal_align_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef HORIZONTAL_ALIGN_ITEM_HH
#define HORIZONTAL_ALIGN_ITEM_HH

#include "item.hh"
#include "align-element.hh"

/**
  Order elems left to right.

  TODO: insert (order, elem)
  */
class Horizontal_align_item : public Item , public Align_element {
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  SCORE_ELEMENT_CLONE(Horizontal_align_item);
  void add_item (Item*, int p);
  Horizontal_align_item();
  virtual void do_print() const;

};
#endif // HORIZONTAL_ALIGN_ITEM_HH
