/*
  score.hh -- declare Score

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCORE_HH
#define SCORE_HH

#include "input.hh"
#include "lily-proto.hh"

#include "parray.hh"
#include "smobs.hh"

class Score : public Input
{
public:
  Link_array<Music_output_def> defs_;
  SCM music_;
  SCM header_;
    
  Score ();
  Score (Score const&);
  DECLARE_SMOBS (Score,foo);
private:
};
DECLARE_UNSMOB(Score,score); 


SCM ly_run_translator (SCM, SCM);
SCM ly_render_output (SCM, SCM, SCM);
void default_rendering (SCM,SCM,SCM,SCM);
#endif
