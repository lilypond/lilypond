/*
  score-scheme.cc --  implement

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/


#include "score.hh"
#include "music.hh"

LY_DEFINE (ly_music_scorify, "ly:music-scorify",
	   2, 0, 0,
	   (SCM music, SCM parser),
	   "Return MUSIC encapsulated in SCORE.")
{
#if 0
  SCM_ASSERT_TYPE (ly_c_music_p (music), music, SCM_ARG1, __FUNCTION__, "music");
#endif
  Score *score = new Score;

  score->set_music (music, parser);

  scm_gc_unprotect_object (score->self_scm ());
  return score->self_scm ();
}
