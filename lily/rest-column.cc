/*
  rest-column.cc -- implement Rest_column

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "rest-column.hh"
#include "note-head.hh"
#include "rest-column.hh"

void
Rest_column::add(Note_head *n_l)
{
    add_support(n_l);
    head_l_arr_.push(n_l);
}

void
Rest_column::translate_y(Real dy_f)
{
    for (int i=0; i < head_l_arr_.size(); i++)
	head_l_arr_[i]->translate_y(dy_f);
}

IMPLEMENT_STATIC_NAME(Rest_column);
IMPLEMENT_IS_TYPE_B1(Rest_column,Item);

Rest_column::Rest_column()
{
    dir_i_ = 0;
}
    

void
Rest_column::do_substitute_dependency(Score_elem*o,Score_elem*n)
{
    Script_column::do_substitute_dependency(o,n);
    if (o->name() == Note_head::static_name()) {
	head_l_arr_.substitute( (Note_head*)o->item(), 
				(n)? (Note_head*)n->item() : 0);
    }
}
