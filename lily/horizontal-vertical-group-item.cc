/*
  horizontal-vertical-group-item.cc -- implement Horizontal_vertical_group_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "horizontal-vertical-group-item.hh"
#include "p-col.hh"

void
Horizontal_vertical_group_item::do_print() const
{
  Horizontal_vertical_group_element::do_print();
}

IMPLEMENT_IS_TYPE_B2(Horizontal_vertical_group_item,Horizontal_vertical_group_element,Item);
