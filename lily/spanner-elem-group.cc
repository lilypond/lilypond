/*
  spanner-elem-group.cc -- implement 

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "p-col.hh"
#include "spanner-elem-group.hh"


IMPLEMENT_STATIC_NAME(Spanner_elem_group);

Interval
Spanner_elem_group::do_width() const
{
    return Spanner::do_width();
}

void
Spanner_elem_group::do_print() const
{
    Element_group::do_print();
}

