//
// lilypond-column.cc -- implement Lilypond_column
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#include "lilypond-column.hh"

Lilypond_column::Lilypond_column (Lilypond_score* lilypond_score_l, Rational mom)
{
  lilypond_score_l_ = lilypond_score_l;
  at_mom_ = mom;
}

void 
Lilypond_column::add_item (Lilypond_item* lilypond_item_l)
{
   lilypond_item_l_list_.append (new Cons<Lilypond_item> (lilypond_item_l, 0));
}

Rational
Lilypond_column::at_mom()
{
  return at_mom_;
}
