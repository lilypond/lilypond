/*
  elem-group-item.cc -- implement Horizontal_vertical_group_item

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "elem-group-item.hh"

void
Horizontal_vertical_group_item::do_print()const
{
    Horizontal_vertical_group::do_print();
}
IMPLEMENT_STATIC_NAME(Horizontal_vertical_group_item);
IMPLEMENT_IS_TYPE_B2(Horizontal_vertical_group_item,Horizontal_vertical_group,Item);
    
IMPLEMENT_IS_TYPE_B2(Horizontal_group_item, Horizontal_group, Item);
IMPLEMENT_STATIC_NAME(Horizontal_group_item);
