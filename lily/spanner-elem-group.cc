/*
  spanner-elem-group.cc -- implement Spanner_elem_group

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "p-col.hh"
#include "spanner-elem-group.hh"



IMPLEMENT_IS_TYPE_B2(Spanner_elem_group,Spanner,Horizontal_vertical_group);

Interval
Spanner_elem_group::do_width() const
{
    return Spanner::do_width();
}

void
Spanner_elem_group::do_print() const
{
#ifndef NPRINT
    Spanner::do_print();
    Horizontal_vertical_group::do_print();
#endif
}

