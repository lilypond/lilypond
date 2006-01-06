/*
  score.hh -- declare Score

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SCORE_HH
#define SCORE_HH

#include "lily-proto.hh"

#include "input.hh"
#include "parray.hh"
#include "smobs.hh"
#include "virtual-methods.hh"
#include "string.hh"

class Score : public Input
{
  DECLARE_SMOBS (Score, foo);

  SCM music_;

public:
  String user_key_;
  Link_array<Output_def> defs_;
  SCM header_;
  bool error_found_;

  Score ();
  Score (Score const &);

  SCM get_music () const;
  void set_music (SCM music);
  SCM book_rendering (Output_def *, Output_def *, Object_key *);
};

DECLARE_UNSMOB (Score, score);

void default_rendering (SCM, SCM, SCM, SCM, SCM, SCM);
SCM ly_render_output (SCM, SCM);
SCM ly_run_translator (SCM, SCM, SCM);

#endif /* SCORE_HH */
