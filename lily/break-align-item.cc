/*
  break-align-item.cc -- implement Break_align_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "break-align-item.hh"

void
Break_align_item::do_pre_processing()
{

  align_i_ = break_status_i();
  Horizontal_align_item::do_pre_processing();
}

IMPLEMENT_IS_TYPE_B1(Break_align_item, Horizontal_align_item);
