/*
  score-column.hh -- declare Score_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCORE_COLUMN_HH
#define SCORE_COLUMN_HH

#include "lily-proto.hh"
#include "array.hh"
#include "moment.hh"
#include "p-col.hh"

/**
  Column with durational info.
  
  The columns which contain data have a rhythmical
  position. Score_column is the type with a rhythmical time attached
  to it. The calculation of idealspacing is done with data in these
  columns. (notably: the #durations# field)

  */

class Score_column : public Paper_column {
  friend class Score;
  friend class Score_engraver;

  bool musical_b_;
  int break_penalty_i_;
  Moment when_;

public:
  int break_penalty_i () { return break_penalty_i_; }
  DECLARE_MY_RUNTIME_TYPEINFO;
  SCORE_ELEMENT_CLONE(Score_column);
  /// length of notes/rests in this column
  Array<Moment> durations;
    

  Moment when() {  return when_; }
  Score_column (Moment when);       
  void add_duration (Moment);
  void preprocess();
  bool musical_b() { return musical_b_; }
  void do_print() const;
};

#endif // SCORE_COLUMN_HH






