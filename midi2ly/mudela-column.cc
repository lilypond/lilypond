//
// mudela-column.cc -- implement Mudela_column
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#include "mudela-column.hh"

Mudela_column::Mudela_column (Mudela_score* mudela_score_l, Rational mom)
{
  mudela_score_l_ = mudela_score_l;
  at_mom_ = mom;
}

void 
Mudela_column::add_item (Mudela_item* mudela_item_l)
{
   mudela_item_l_list_.append (new Cons<Mudela_item> (mudela_item_l, 0));
}

Rational
Mudela_column::at_mom()
{
  return at_mom_;
}
