/*
  paper-score.hh -- declare Paper_score

  source file of the GNU LilyPond music typesetter

  (c) 1996--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef PAPER_SCORE_HH
#define PAPER_SCORE_HH

#include "column-x-positions.hh"
#include "parray.hh"
#include "lily-proto.hh"
#include "music-output.hh"
#include "lily-guile.hh"
#include "protected-scm.hh"

/* PAPER output */
class Paper_score : public Music_output
{
public:
  Output_def *paper_;
  System *system_;

  Paper_score ();

  int find_col_idx (Paper_column const *) const;
  Link_array<Item> broken_col_range (Item const*, Item const*) const;
  void typeset_line (System*);
  void output ();

protected:
  virtual SCM process (String);

private:
  Protected_scm systems_;

  void preprocess ();
  void calc_idealspacing ();
  Array<Column_x_positions> calc_breaking ();
  void postprocess ();
  Paper_score (Paper_score const &);
};

#endif /* PAPER_SCORE_HH */
