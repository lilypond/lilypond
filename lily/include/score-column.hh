/*
  score-column.hh -- declare Score_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCORE_COLUMN_HH
#define SCORE_COLUMN_HH

#include "lily-proto.hh"
#include "array.hh"
#include "moment.hh"
#include "paper-column.hh"

/**
  Column with durational info.
  
  The columns which contain data have a rhythmical
  position. Score_column is the type with a rhythmical time attached
  to it. The calculation of idealspacing is done with data in these
  columns.

  */

class Score_column : public Paper_column {
  Moment when_;
public:
  Moment shortest_playing_mom_;
  Moment shortest_starter_mom_;

  
  VIRTUAL_COPY_CONS(Score_element);

  Moment when_mom() { return when_; }
  Score_column (Moment when);

  bool musical_b() const;
  void do_print() const;
};

#endif // SCORE_COLUMN_HH






