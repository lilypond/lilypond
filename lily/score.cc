/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2014 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <cstdio>

using namespace std;

#include "book.hh"
#include "cpu-timer.hh"
#include "global-context.hh"
#include "international.hh"
#include "lily-parser.hh"
#include "main.hh"
#include "music.hh"
#include "music.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "paper-score.hh"
#include "warn.hh"


Input *
Score::origin () const
{
  return Input::unsmob (input_location_);
}

Score::Score ()
{
  header_ = SCM_EOL;
  music_ = SCM_EOL;
  input_location_ = SCM_EOL;

  error_found_ = false;

  smobify_self ();
  input_location_ = Input ().smobbed_copy ();
}

Score::~Score ()
{
}

const char Score::type_p_name_[] = "ly:score?";

SCM
Score::mark_smob ()
{
  scm_gc_mark (header_);
  for (vsize i = defs_.size (); i--;)
    scm_gc_mark (defs_[i]->self_scm ());

  scm_gc_mark (input_location_);
  return music_;
}

Score::Score (Score const &s)
  : Smob<Score> ()
{
  header_ = SCM_EOL;
  music_ = SCM_EOL;
  input_location_ = SCM_EOL;
  error_found_ = s.error_found_;

  smobify_self ();
  input_location_ = s.origin ()->smobbed_copy ();

  Music *m = Music::unsmob (s.music_);
  if (m)
    {
      Music *mclone = m->clone ();
      music_ = mclone->unprotect ();
    }
  else
    music_ = SCM_EOL;

  for (vsize i = 0, n = s.defs_.size (); i < n; i++)
    {
      Output_def *copy = s.defs_[i]->clone ();
      defs_.push_back (copy);
      copy->unprotect ();
    }
  header_ = ly_make_module (false);
  if (ly_is_module (s.header_))
    ly_module_copy (header_, s.header_);
}

/*
  Format score, return list of Music_output objects.

  LAYOUTBOOK should be scaled already.
*/
SCM
Score::book_rendering (Output_def *layoutbook,
                       Output_def *default_def)
{
  if (error_found_)
    return SCM_EOL;

  Real scale = 1.0;

  if (layoutbook && layoutbook->c_variable ("is-paper") == SCM_BOOL_T)
    scale = scm_to_double (layoutbook->c_variable ("output-scale"));

  SCM outputs = SCM_EOL;

  int outdef_count = defs_.size ();

  for (int i = 0; !i || i < outdef_count; i++)
    {
      Output_def *def = outdef_count ? defs_[i] : default_def;
      SCM scaled = def->self_scm ();

      if (def->c_variable ("is-layout") == SCM_BOOL_T)
        {
          def = scale_output_def (def, scale);
          def->parent_ = layoutbook;

          scaled = def->unprotect ();
        }

      /* TODO: fix or junk --no-layout.  */
      SCM context = ly_run_translator (music_, scaled);
      if (dynamic_cast<Global_context *> (Context::unsmob (context)))
        {
          SCM s = ly_format_output (context);

          outputs = scm_cons (s, outputs);
        }

      scm_remember_upto_here_1 (scaled);
    }

  return scm_reverse_x (outputs, SCM_EOL);
}

void
Score::set_music (SCM music)
{
  if (Music::is_smob (music_))
    {
      Music::unsmob (music)->origin ()->error (_ ("already have music in score"));
      Music::unsmob (music_)->origin ()->error (_ ("this is the previous music"));
    }
  Music *m = Music::unsmob (music);
  if (m && to_boolean (m->get_property ("error-found")))
    {
      m->origin ()->error (_ ("errors found, ignoring music expression"));

      this->error_found_ = this->error_found_
                           || to_boolean (m->get_property ("error-found"));
    }

  if (this->error_found_)
    this->music_ = SCM_EOL;
  else
    this->music_ = music;
}

SCM
Score::get_music () const
{
  return music_;
}

void
Score::add_output_def (Output_def *def)
{
  defs_.push_back (def);
}

SCM
Score::get_header () const
{
  return header_;
}

void
Score::set_header (SCM module)
{
  assert (ly_is_module (module));
  header_ = module;
}
