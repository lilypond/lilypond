//
// mudela-column.hh -- declare Mudela_column
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef MUDELA_COLUMN_HH
#define MUDELA_COLUMN_HH

#include "proto.hh"
#include "midi2ly-proto.hh"
#include "rational.hh"
#include "cons.hh"

/// (mudela_column)
class Mudela_column 
{
public:
    Mudela_column (Mudela_score* mudela_score_l, Rational mom);

    void add_item (Mudela_item* mudela_item_l);
    Rational at_mom ();

    Cons_list<Mudela_item> mudela_item_l_list_;
    Rational at_mom_;
    Mudela_score* mudela_score_l_;
};

#endif // MUDELA_COLUMN_HH

