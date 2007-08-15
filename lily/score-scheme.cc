/*
  score-scheme.cc -- implement Score bindings.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "score.hh"

#include "music.hh"
#include "output-def.hh"
#include "global-context.hh"
#include "lilypond-key.hh"

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

LY_DEFINE (ly_score_process, "ly:score-process",
	   5, 0, 0,
	   (SCM score_smob,
	    SCM default_header,
	    SCM default_paper,
	    SCM default_layout,
	    SCM basename),
	   "Print score without page-layout: just print the systems.")
{
  Score *score = unsmob_score (score_smob);

  SCM_ASSERT_TYPE (score, score_smob, SCM_ARG1, __FUNCTION__, "score");

  // allow header to be undefined.
  SCM_ASSERT_TYPE (unsmob_output_def (default_paper),
		   default_header, SCM_ARG3, __FUNCTION__, "\\paper block");
  SCM_ASSERT_TYPE (unsmob_output_def (default_layout),
		   default_header, SCM_ARG4, __FUNCTION__, "\\layout block");

  Object_key *key = new Lilypond_general_key (0, score->user_key_, 0);

  if (score->error_found_)
    return SCM_UNSPECIFIED;

  SCM header = ly_is_module (score->header_)
    ? score->header_
    : default_header;

  for (vsize i = 0; i < score->defs_.size (); i++)
    default_rendering (score->get_music (), score->defs_[i]->self_scm (),
		       default_paper, header, basename, key->self_scm ());

  if (score->defs_.empty ())
    {
      default_rendering (score->get_music (),
			 default_layout,
			 default_paper,
			 header, basename, key->self_scm ());
    }

  key->unprotect ();
  return SCM_UNSPECIFIED;
}

