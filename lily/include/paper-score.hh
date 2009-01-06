/*
  paper-score.hh -- declare Paper_score

  source file of the GNU LilyPond music typesetter

  (c) 1996--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef PAPER_SCORE_HH
#define PAPER_SCORE_HH

#include "column-x-positions.hh"
#include "music-output.hh"

/* LAYOUT output */
class Paper_score : public Music_output
{
  Output_def *layout_;
  System *system_;
  SCM systems_;
  SCM paper_systems_;

  mutable vector<Grob*> cols_;
  mutable vector<vsize> break_indices_;
public:
  Paper_score (Output_def *);
  
  DECLARE_CLASSNAME(Paper_score);

  Output_def *layout () const;
  System *root_system () const;

  void typeset_system (System *);
  vector<Column_x_positions> calc_breaking ();
  vector<vsize> find_break_indices () const;
  vector<vsize> get_break_indices () const;
  vector<Grob*> get_columns () const;
  SCM get_paper_systems ();
protected:
  virtual void process ();
  virtual void derived_mark () const;

private:
  Paper_score (Paper_score const &);
};

#endif /* PAPER_SCORE_HH */
