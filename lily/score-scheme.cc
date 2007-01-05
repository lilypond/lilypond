/*
  score-scheme.cc -- implement Score bindings.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "score.hh"

#include "music.hh"
#include "output-def.hh"
#include "global-context.hh"
#include "lilypond-key.hh"
#include "music-output.hh"
#include "paper-score.hh"
#include "paper-book.hh"

LY_DEFINE (ly_make_score, "ly:make-score",
	   1, 0, 0,
	   (SCM music),
	   "Return score with @var{music} encapsulated in @var{score}.")
{
  Music *mus = unsmob_music (music);
  SCM_ASSERT_TYPE (mus, music, SCM_ARG1, __FUNCTION__, "music");

  Score *score = new Score;
  score->set_music (music);

  return score->unprotect ();
}

LY_DEFINE (ly_score_output_defs, "ly:score-output-defs",
	   1, 0, 0, (SCM score),
	   "All output defs in a score.")
{
  Score *sc = unsmob_score (score);
  SCM_ASSERT_TYPE (sc, score, SCM_ARG1, __FUNCTION__, "score");

  SCM l = SCM_EOL;
  for (vsize i = 0; i < sc->defs_.size (); i++)
    l = scm_cons (sc->defs_[i]->self_scm(), l);
  return scm_reverse_x (l, SCM_EOL);
}



LY_DEFINE (ly_score_header, "ly:score-header",
	   1, 0, 0, (SCM score),
	   "return score header.")
{
  Score *sc = unsmob_score (score);
  SCM_ASSERT_TYPE (sc, score, SCM_ARG1, __FUNCTION__, "score");
  return sc->header_;
}


LY_DEFINE (ly_score_music, "ly:score-music",
	   1, 0, 0, (SCM score),
	   "return score music.")
{
  Score *sc = unsmob_score (score);
  SCM_ASSERT_TYPE (sc, score, SCM_ARG1, __FUNCTION__, "score");
  return sc->get_music ();
}

LY_DEFINE (ly_score_error_p, "ly:score-error?",
	   1, 0, 0, (SCM score),
	   "Was there an error in the score?")
{
  Score *sc = unsmob_score (score);
  SCM_ASSERT_TYPE (sc, score, SCM_ARG1, __FUNCTION__, "score");
  return scm_from_bool (sc->error_found_);
}

LY_DEFINE (ly_score_embedded_format, "ly:score-embedded-format",
	   2, 1, 0, (SCM score, SCM layout, SCM key),
	   "Run @var{score} through @var{layout}, an output definition, "
	   "scaled to correct output-scale already, "
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
  for (vsize i = 0; !score_def && i < sc->defs_.size (); i++)
    if (sc->defs_[i]->c_variable ("is-layout") == SCM_BOOL_T)
      score_def = sc->defs_[i];

  if (!score_def)
    return SCM_BOOL_F;

  score_def = score_def->clone ();
  SCM prot = score_def->unprotect ();

  /* TODO: SCORE_DEF should be scaled according to OD->parent_ or OD
     itself. */
  score_def->parent_ = od;

  SCM context = ly_run_translator (sc->get_music (), score_def->self_scm (),
				   key);
  SCM output = ly_format_output (context);

  scm_remember_upto_here_1 (prot);
  return output;
}
