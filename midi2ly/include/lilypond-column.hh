//
// lilypond-column.hh -- declare Lilypond_column
//
// (c) 1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef LILYPOND_COLUMN_HH
#define LILYPOND_COLUMN_HH

#include "flower-proto.hh"
#include "midi2ly-proto.hh"
#include "rational.hh"
#include "cons.hh"

/// (lilypond_column)
class Lilypond_column 
{
public:
  Lilypond_column (Lilypond_score* lilypond_score_l, Rational mom);

  void add_item (Lilypond_item* lilypond_item_l);
  Rational at_mom ();

  Cons_list<Lilypond_item> lilypond_item_l_list_;
  Rational at_mom_;
  Lilypond_score* lilypond_score_l_;
};

#endif // LILYPOND_COLUMN_HH

