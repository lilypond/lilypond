/*
  score.hh -- declare Score

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef SCORE_HH
#define SCORE_HH

#include "lily-proto.hh"

#include "input.hh"
#include "parray.hh"
#include "smobs.hh"
#include "virtual-methods.hh"

class Score : public Input
{
  DECLARE_SMOBS (Score, foo);

  SCM music_;
public:
  Link_array<Output_def> defs_;
  SCM header_;
  bool error_found_;
  
  SCM get_music () const;
  void set_music (SCM music, SCM parser);
  Score ();
  Score (Score const&);
  SCM book_rendering (String, Output_def*, Output_def*);
};

DECLARE_UNSMOB (Score, score);

SCM ly_run_translator (SCM, SCM);
SCM ly_render_output (SCM, SCM);
void default_rendering (SCM, SCM, SCM, SCM, SCM);

#endif /* SCORE_HH */
