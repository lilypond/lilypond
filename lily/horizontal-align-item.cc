/*
  horizontal-align-item.cc -- implement Horizontal_align_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "horizontal-align-item.hh"
#include "debug.hh"

IMPLEMENT_IS_TYPE_B2(Horizontal_align_item,Align_element,Item);

void
Horizontal_align_item::add_item (Item *i,int p)
{
  add_element_priority (i,p);
}

void
Horizontal_align_item::do_print() const
{
#ifndef NPRINT
  Item::do_print ();
  Align_element::do_print ();
#endif
}


Horizontal_align_item::Horizontal_align_item ()
{
  align_dir_ = CENTER;
  stacking_dir_ = RIGHT;
  axis_ = X_AXIS;
}
