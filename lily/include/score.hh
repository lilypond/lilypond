/*
  score.hh -- declare Score

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SCORE_HH
#define SCORE_HH

#include "lily-proto.hh"

#include "input.hh"
#include "std-vector.hh"
#include "smobs.hh"
#include "virtual-methods.hh"

class Score
{
  DECLARE_SMOBS (Score);

  SCM music_;
  SCM input_location_;
  SCM header_;
public:
  Input *origin() const;
 
  vector<Output_def*> defs_;
  string user_key_;
  bool error_found_;

  Score ();
  Score (Score const &);

  VIRTUAL_COPY_CONSTRUCTOR (Score, Score);
  
  SCM get_music () const;
  void add_output_def (Output_def *def);
  void set_music (SCM music);
  SCM book_rendering (Output_def *, Output_def *);
  SCM get_header () const;
  void set_header (SCM module);
};

DECLARE_UNSMOB (Score, score);

SCM ly_render_output (SCM, SCM);
SCM ly_run_translator (SCM, SCM);

#endif /* SCORE_HH */
