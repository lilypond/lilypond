/*
  rest-column.cc -- implement Rest_column

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "rest-column.hh"
#include "notehead.hh"
#include "rest-column.hh"

void
Rest_column::add(Notehead *n_l)
{
    add_support(n_l);
    head_l_arr_.push(n_l);
}

void
Rest_column::translate_y(Real dy_f)
{
    for (int i=0; i < head_l_arr_.size(); i++)
	head_l_arr_[i]->translate(Offset(0,dy_f));
}

IMPLEMENT_STATIC_NAME(Rest_column);

Rest_column::Rest_column()
{
    dir_i_ = 0;
}
    
