/*
  spanner-elem-group.cc -- implement 

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "p-col.hh"
#include "spanner-elem-group.hh"

void
Spanner_elem_group::do_break_at(PCol*c1, PCol*c2 )
{
    Line_of_score * line_C = c1->line_l_;
    Array<Score_elem*>  old_elems=elem_l_arr_;
    elem_l_arr_.set_size(0);
    for (int i=0; i < old_elems.size(); i++) {
	if (old_elems[i]->line_l() == line_C) {
	    add_element(old_elems[i]);
	}
    }
}

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

