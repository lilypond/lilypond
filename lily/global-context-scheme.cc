/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "global-context.hh"
#include "international.hh"
#include "music-output.hh"
#include "music.hh"
#include "output-def.hh"
#include "translator-group.hh"
#include "warn.hh"

LY_DEFINE (ly_format_output, "ly:format-output",
           1, 0, 0, (SCM context),
           "Given a global context in its final state,"
           " process it and return the @code{Music_output} object"
           " in its final state.")
{
  auto *const g = LY_ASSERT_SMOB (Global_context, context, 1);

  SCM output = g->get_output ();
  progress_indication ("\n");

  if (Music_output *od = unsmob<Music_output> (output))
    od->process ();

  return output;
}

LY_DEFINE (ly_make_global_translator, "ly:make-global-translator",
           1, 0, 0, (SCM global),
           "Create a translator group and connect it to the global context"
           " @var{global}.  The translator group is returned.")
{
  auto *const g = LY_ASSERT_SMOB (Global_context, global, 1);

  Translator_group *tg = new Translator_group ();
  tg->connect_to_context (g);
  g->implementation_ = tg;

  return tg->unprotect ();
}

LY_DEFINE (ly_make_global_context, "ly:make-global-context",
           1, 0, 0, (SCM output_def),
           "Set up a global interpretation context, using the output"
           " block @var{output-def}.  The context is returned.")
{
  auto *const odef = LY_ASSERT_SMOB (Output_def, output_def, 1);

  Global_context *glob = new Global_context (odef);

  if (!glob)
    {
      programming_error ("no toplevel translator");
      return SCM_BOOL_F;
    }

  return glob->unprotect ();
}

LY_DEFINE (ly_interpret_music_expression, "ly:interpret-music-expression",
           2, 0, 0, (SCM mus, SCM ctx),
           "Interpret the music expression @var{mus} in the global context"
           " @var{ctx}.  The context is returned in its final state.")
{
  auto *const music = LY_ASSERT_SMOB (Music, mus, 1);
  auto *const g = LY_ASSERT_SMOB (Global_context, ctx, 2);
  g->iterate (music, true);
  return ctx;
}

LY_DEFINE (ly_run_translator, "ly:run-translator",
           2, 1, 0, (SCM mus, SCM output_def),
           "Process @var{mus} according to @var{output-def}.  An"
           " interpretation context is set up, and @var{mus} is"
           " interpreted with it.  The context is returned in its"
           " final state.\n"
           "\n"
           "Optionally, this routine takes an object-key to"
           " to uniquely identify the score block containing it.")
{
  auto *const music = LY_ASSERT_SMOB (Music, mus, 1);
  LY_ASSERT_SMOB (Output_def, output_def, 2);

  SCM glob = ly_make_global_context (output_def);
  if (auto *g = unsmob<Global_context> (glob))
    {
      ly_make_global_translator (glob);
      if (!g->iterate (music, false))
        {
          music->warning (_ ("skipping zero-duration score"));
          music->warning (_ ("to suppress this, "
                             "consider adding a spacer rest"));
        }
    }
  else
    {
      programming_error ("failed to create global context");
    }

  return glob;
}
