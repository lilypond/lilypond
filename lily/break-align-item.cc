/*
  break-align-item.cc -- implement Break_align_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "break-align-item.hh"
#include "dimensions.hh"

void
Break_align_item::do_pre_processing()
{
  align_dir_ = break_status_dir();
  threshold_interval_[SMALLER] = 1.5 PT;
  flip (&align_dir_);
  Horizontal_align_item::do_pre_processing();
}

IMPLEMENT_IS_TYPE_B1(Break_align_item, Horizontal_align_item);
