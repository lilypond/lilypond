/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"
#include "dispatcher.hh"
#include "engraver.hh"
#include "grob-properties.hh"
#include "listener.hh"
#include "stream-event.hh"
#include "translator.icc"
#include "warn.hh"

class Grace_engraver : public Engraver
{
  Moment last_moment_;
  SCM grace_settings_;
  void consider_change_grace_settings ();

protected:
  void derived_mark () const override;
  virtual void process_music ();
  virtual void start_translation_timestep ();
  void finalize () override;

  TRANSLATOR_DECLARATIONS (Grace_engraver);
  void grace_change (SCM);
};

Grace_engraver::Grace_engraver (Context *c) : Engraver (c)
{
  grace_settings_ = SCM_EOL;
  last_moment_ = Moment (Rational (-1, 1));
}

// The iterator should usually come before process_music
void Grace_engraver::grace_change (SCM) { consider_change_grace_settings (); }

// if we are in grace time already on initialization, it is unlikely
// that we'll receive a GraceChange event from the grace iterator yet,
// so we want to start into grace mode anyway.  The downside is that
// this will get us confused when given something like
//
// \new Voice { \oneVoice \grace { c''8 8 } g'1 }
//
// where \grace executes its actions already before \oneVoice, causing
// different stem directions.

void
Grace_engraver::start_translation_timestep ()
{
  // Only on startup
  if (last_moment_ == Rational (-1))
    consider_change_grace_settings ();
}

// If the grace iterator has moved off to some other context, we might
// not get to see the ChangeContext event.  In that case, we still
// want to change into or out of grace mode settings as appropriate

void
Grace_engraver::process_music ()
{
  // We may have lost connection to the iterator in which case we
  // still need to call consider_change_grace_settings in particular
  // in order to get out of grace mode again
  if (last_moment_ != now_mom ())
    consider_change_grace_settings ();
}

void
Grace_engraver::consider_change_grace_settings ()
{
  Moment now = now_mom ();

  if (!now.grace_part_)
    {
      for (SCM s = grace_settings_; scm_is_pair (s); s = scm_cdr (s))
        {
          SCM elt = scm_car (s);
          SCM context = scm_car (elt);
          SCM grob = scm_cadr (elt);
          SCM cell = scm_cddr (elt);

          Grob_property_info (unsmob<Context> (context), grob)
              .matched_pop (cell);
        }
      grace_settings_ = SCM_EOL;
    }
  else if (!last_moment_.grace_part_)
    {
      SCM settings = get_property ("graceSettings");

      grace_settings_ = SCM_EOL;
      for (SCM s = settings; scm_is_pair (s); s = scm_cdr (s))
        {
          SCM entry = scm_car (s);
          SCM context_name = scm_car (entry);
          SCM grob = scm_cadr (entry);
          SCM sym = scm_caddr (entry);
          SCM val = scm_cadr (scm_cddr (entry));

          if (!scm_is_pair (sym))
            sym = scm_list_1 (sym);

          Context *c = find_context_above (context (), context_name);
          if (c)
            {
              SCM cell = Grob_property_info (c, grob).push (sym, val);
              grace_settings_ = scm_cons (
                  scm_cons2 (c->self_scm (), grob, cell), grace_settings_);
            }
          else
            programming_error ("cannot find context from graceSettings: "
                               + ly_symbol2string (context_name));
        }
    }
  if (last_moment_ == Rational (-1))
    {
      Dispatcher *d = context ()->event_source ();
      d->add_listener (GET_LISTENER (Grace_engraver, grace_change),
                       ly_symbol2scm ("GraceChange"));
    }
  last_moment_ = now;
}

void
Grace_engraver::finalize ()
{
  if (last_moment_ != Rational (-1))
    {
      Dispatcher *d = context ()->event_source ();
      d->remove_listener (GET_LISTENER (Grace_engraver, grace_change),
                          ly_symbol2scm ("GraceChange"));
    }
}

void
Grace_engraver::derived_mark () const
{
  scm_gc_mark (grace_settings_);
  Engraver::derived_mark ();
}

void
Grace_engraver::boot ()
{
}

ADD_TRANSLATOR (Grace_engraver,
                /* doc */
                "Set font size and other properties for grace notes.",

                /* create */
                "",

                /* read */
                "graceSettings ",

                /* write */
                "");
