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


IMPLEMENT_IS_TYPE_B1(Rest_column,Head_column);


/**
  translate the rest symbols
 */
void
Rest_column::translate_heads (int dy_i)
{
    for (int i=0; i < head_l_arr_.size(); i++)
	head_l_arr_[i]->position_i_ += dy_i;
}

