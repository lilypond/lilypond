/*
  score-scheme.cc -- implement Score bindings.

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "score.hh"
#include "music.hh"
#include "output-def.hh"
#include "global-context.hh"

LY_DEFINE (ly_music_scorify, "ly:music-scorify",
	   2, 0, 0,
	   (SCM music, SCM parser),
	   "Return MUSIC with TEXTS encapsulated in SCORE.")
{
#if 0
  SCM_ASSERT_TYPE (ly_c_music_p (music), music, SCM_ARG1, __FUNCTION__, "music");
#endif
  Score *score = new Score;
  score->set_music (music, parser);
  scm_gc_unprotect_object (score->self_scm ());
  return score->self_scm ();
}

LY_DEFINE (ly_score_embedded_format, "ly:score-embedded-format",
	   2, 1, 0, (SCM score, SCM layout, SCM key),
	   "Run @var{score} through @var{layout}, an output definition, "
	   "scaled to correct outputscale already, "
	   "return a list of layout-lines. "
	   "\nTake optional Object_key argument.")
{
  Score *sc = unsmob_score (score);
  Output_def *od = unsmob_output_def (layout);

  if (sc->error_found_)
    return SCM_EOL;

  SCM_ASSERT_TYPE (sc, score, SCM_ARG1, __FUNCTION__, "Score");
  SCM_ASSERT_TYPE (od, layout, SCM_ARG2, __FUNCTION__, "Output_def");

  Output_def *score_def = 0;

  /* UGR, FIXME, these are default \layout blocks once again.  They
     suck. */
  for (int i = 0; !score_def && i < sc->defs_.size (); i++)
    if (sc->defs_[i]->c_variable ("is-layout") == SCM_BOOL_T)
      score_def = sc->defs_[i];

  if (!score_def)
    return scm_c_make_vector (0, SCM_EOL);

  score_def = score_def->clone ();
  SCM prot = score_def->self_scm ();
  scm_gc_unprotect_object (prot);

  /* TODO: SCORE_DEF should be scaled according to OD->parent_ or OD
     itself. */
  score_def->parent_ = od;

  SCM context = ly_run_translator (sc->get_music (), score_def->self_scm (),
				   key);
  SCM lines = ly_format_output (context);

  scm_remember_upto_here_1 (prot);
  return lines;
}
