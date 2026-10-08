/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "engraver.hh"
#include "context.hh"
#include "dispatcher.hh"
#include "listener.hh"
#include "warn.hh"
#include "grob-properties.hh"
#include "stream-event.hh"
#include "translator.icc"

class Grace_engraver final : public Engraver
{
  bool in_grace_ = false;
  SCM grace_settings_ = SCM_EOL;
  void consider_change_grace_settings (bool in_grace);

  void derived_mark () const override;
  void process_music ();
  void initialize () override;
  void finalize () override;

  TRANSLATOR_DECLARATIONS (Grace_engraver);
  void grace_change (SCM);
};

Grace_engraver::Grace_engraver (Context *c)
  : Engraver (c)
{
}

// The iterator should usually come before process_music
void
Grace_engraver::grace_change (SCM ev)
{
  bool in_grace = from_scm<bool> (get_property (unsmob<Stream_event> (ev), "in-grace"));
  consider_change_grace_settings (in_grace);
}

void
Grace_engraver::initialize ()
{
  Dispatcher *d = context ()->event_source ();
  d->add_listener (GET_LISTENER (this, grace_change),
                   ly_symbol2scm ("GraceChange"));
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
  consider_change_grace_settings (now_mom ().grace_part_ != 0);
}

void
Grace_engraver::consider_change_grace_settings (bool in_grace)
{
  if (in_grace == in_grace_)
    return;

  if (in_grace)
    {
      SCM settings = get_property (this, "graceSettings");

      grace_settings_ = SCM_EOL;
      for (SCM s = settings; scm_is_pair (s); s = scm_cdr (s))
        {
          SCM entry = scm_car (s);
          SCM context_name = scm_car (entry);
          SCM grob = scm_cadr (entry);
          SCM sym = scm_caddr (entry);
          SCM val = scm_cadr (scm_cddr (entry));

          if (!scm_is_pair (sym))
            sym = ly_list (sym);

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
  else
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

  in_grace_ = in_grace;
}

void
Grace_engraver::finalize ()
{
  Dispatcher *d = context ()->event_source ();
  d->remove_listener (GET_LISTENER (this, grace_change),
                      ly_symbol2scm ("GraceChange"));
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
                R"(
Set font size and other properties for grace notes.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
graceSettings
                )",

                /* write */
                R"(

                )");
