/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

LY_DEFINE (ly_make_score, "ly:make-score", 1, 0, 0, (SCM music),
           R"(
Return score with @var{music} encapsulated in it.
           )")
{
  LY_ASSERT_SMOB (Music, music, 1);

  Score *score = new Score;
  score->set_music (music);

  return score->unprotect ();
}

LY_DEFINE (ly_score_output_defs, "ly:score-output-defs", 1, 0, 0, (SCM score),
           R"(
All output definitions in a score.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Score, score, 1);

  SCM l = SCM_EOL;
  for (vsize i = 0; i < sc->defs_.size (); i++)
    l = scm_cons (sc->defs_[i]->self_scm (), l);
  return scm_reverse_x (l, SCM_EOL);
}

LY_DEFINE (ly_score_add_output_def_x, "ly:score-add-output-def!", 2, 0, 0,
           (SCM score, SCM def),
           R"(
Add an output definition @var{def} to @var{score}.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Score, score, 1);
  auto *const output_def = LY_ASSERT_SMOB (Output_def, def, 2);
  sc->add_output_def (output_def);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_score_header, "ly:score-header", 1, 0, 0, (SCM score),
           R"(
Return score header.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Score, score, 1);
  return sc->get_header ();
}

LY_DEFINE (ly_score_set_header_x, "ly:score-set-header!", 2, 0, 0,
           (SCM score, SCM module),
           R"(
Set the score header.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Score, score, 1);
  SCM_ASSERT_TYPE (ly_is_module (module), module, SCM_ARG2, __FUNCTION__,
                   "module");

  sc->set_header (module);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_score_music, "ly:score-music", 1, 0, 0, (SCM score),
           R"(
Return score music.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Score, score, 1);
  return sc->get_music ();
}

LY_DEFINE (ly_score_error_p, "ly:score-error?", 1, 0, 0, (SCM score),
           R"(
Was there an error in the score?
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Score, score, 1);
  return to_scm (sc->error_found_);
}

LY_DEFINE (ly_score_embedded_format, "ly:score-embedded-format", 2, 0, 0,
           (SCM score, SCM layout),
           R"(
Run @var{score} through @var{layout} (an output definition) scaled to correct
@code{output-scale} already, returning a list of layout lines.
           )")
{
  auto *const sc = LY_ASSERT_SMOB (Score, score, 1);
  auto *const od = LY_ASSERT_SMOB (Output_def, layout, 2);

  if (sc->error_found_)
    return SCM_EOL;

  Output_def *score_def = 0;

  /* UGR, FIXME, these are default \layout blocks once again.  They
     suck. */
  for (vsize i = 0; !score_def && i < sc->defs_.size (); i++)
    if (scm_is_eq (sc->defs_[i]->c_variable ("output-def-kind"),
                   ly_symbol2scm ("layout")))
      {
        score_def = sc->defs_[i];
      }

  if (!score_def)
    return SCM_BOOL_F;

  /* Don't rescale if the layout has already been scaled */
  if (from_scm<bool> (score_def->c_variable ("cloned")))
    score_def = score_def->clone ();
  else
    score_def = scale_output_def (score_def, output_scale (od));

  score_def->parent_ = od;

  SCM context = ly_run_translator (sc->get_music (), score_def->unprotect ());
  SCM output = ly_format_output (context);

  return output;
}
