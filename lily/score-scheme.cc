/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "score.hh"

#include "music.hh"
#include "output-def.hh"
#include "global-context.hh"
#include "music-output.hh"
#include "paper-score.hh"
#include "paper-book.hh"

LY_DEFINE (ly_make_score, "ly:make-score",
	   1, 0, 0,
	   (SCM music),
	   "Return score with @var{music} encapsulated in @var{score}.")
{
  LY_ASSERT_SMOB (Music, music, 1);

  Score *score = new Score;
  score->set_music (music);

  return score->unprotect ();
}

LY_DEFINE (ly_score_output_defs, "ly:score-output-defs",
	   1, 0, 0, (SCM score),
	   "All output definitions in a score.")
{
  LY_ASSERT_SMOB (Score, score, 1);
  Score *sc = unsmob_score (score);

  SCM l = SCM_EOL;
  for (vsize i = 0; i < sc->defs_.size (); i++)
    l = scm_cons (sc->defs_[i]->self_scm (), l);
  return scm_reverse_x (l, SCM_EOL);
}

LY_DEFINE (ly_score_add_output_def_x, "ly:score-add-output-def!",
	   2, 0, 0, (SCM score, SCM def),
	   "Add an output definition @var{def} to @var{score}.")
{
  LY_ASSERT_SMOB (Score, score, 1);
  LY_ASSERT_SMOB (Output_def, def, 2);
  Score *sc = unsmob_score (score);
  Output_def *output_def = unsmob_output_def (def);
  sc->add_output_def (output_def);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_score_header, "ly:score-header",
	   1, 0, 0, (SCM score),
	   "Return score header.")
{
  LY_ASSERT_SMOB (Score, score, 1);
  Score *sc = unsmob_score (score);
  return sc->get_header ();
}


LY_DEFINE (ly_score_set_header_x, "ly:score-set-header!",
	   2, 0, 0, (SCM score, SCM module),
	   "Set the score header.")
{
  LY_ASSERT_SMOB (Score, score, 1);
  SCM_ASSERT_TYPE (ly_is_module (module), module, SCM_ARG2, __FUNCTION__,
		   "module");
  
  Score *sc = unsmob_score (score);
  sc->set_header (module);
  return SCM_UNSPECIFIED;
}


LY_DEFINE (ly_score_music, "ly:score-music",
	   1, 0, 0, (SCM score),
	   "Return score music.")
{
  LY_ASSERT_SMOB (Score, score, 1);
  Score *sc = unsmob_score (score);
  return sc->get_music ();
}

LY_DEFINE (ly_score_error_p, "ly:score-error?",
	   1, 0, 0, (SCM score),
	   "Was there an error in the score?")
{
  LY_ASSERT_SMOB (Score, score, 1);
  Score *sc = unsmob_score (score);
  return scm_from_bool (sc->error_found_);
}

LY_DEFINE (ly_score_embedded_format, "ly:score-embedded-format",
	   2, 0, 0, (SCM score, SCM layout),
	   "Run @var{score} through @var{layout} (an output definition)"
	   " scaled to correct output-scale already, returning a list of"
	   " layout-lines.  This function takes an optional"
	   " @code{Object_key} argument.")
{
  LY_ASSERT_SMOB (Score, score, 1);
  LY_ASSERT_SMOB (Output_def, layout, 2);

  Score *sc = unsmob_score (score);
  Output_def *od = unsmob_output_def (layout);

  if (sc->error_found_)
    return SCM_EOL;

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

  SCM context = ly_run_translator (sc->get_music (), score_def->self_scm ());
  SCM output = ly_format_output (context);

  scm_remember_upto_here_1 (prot);
  return output;
}
