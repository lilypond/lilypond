/*
  rest-column.cc -- implement Rest_column

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "rest-column.hh"
#include "note-head.hh"
#include "rest-column.hh"
#include "stem.hh"

IMPLEMENT_STATIC_NAME(Rest_column);
IMPLEMENT_IS_TYPE_B1(Rest_column,Item);

Rest_column::Rest_column()
{
    dir_i_ = 0;
    stem_l_ = 0;
}
    
void
Rest_column::add(Note_head *n_l)
{
    add_support(n_l);
    head_l_arr_.push(n_l);
}

void
Rest_column::add(Stem*stem_l)
{
    stem_l_ = stem_l;
    add_dependency(stem_l);
//    add_support(stem_l);
}

void
Rest_column::do_print() const
{
#ifndef NPRINT
    mtor << "heads: " << head_l_arr_.size();
#endif
}

void
Rest_column::do_substitute_dependency(Score_elem*o,Score_elem*n)
{
    Script_column::do_substitute_dependency(o,n);
    if (o == stem_l_)
	stem_l_ = n? (Stem*)n->item() :0;
    
    if (o->is_type_b( Note_head::static_name()) ) 
	head_l_arr_.substitute( (Note_head*)o->item(), 
				(n)? (Note_head*)n->item() : 0);
}


/**
  translate the rest symbols
 */
void
Rest_column::translate_heads(int dy_i)
{
    for (int i=0; i < head_l_arr_.size(); i++)
	head_l_arr_[i]->position_i_ += dy_i;
}

