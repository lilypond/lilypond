/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "item.hh"
#include "context.hh"
#include "duration.hh"
#include "engraver.hh"
#include "international.hh"
#include "note-column.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "script-interface.hh"
#include "stem.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

#include <cctype>

using std::vector;

class Drum_notes_engraver : public Engraver
{
  vector<Item *> scripts_;
  vector<Stream_event *> events_;

public:
  TRANSLATOR_DECLARATIONS (Drum_notes_engraver);

protected:
  void process_music ();
  void acknowledge_stem (Grob_info);
  void acknowledge_note_column (Grob_info_t<Item>);
  void listen_note (Stream_event *);
  void stop_translation_timestep ();
};

Drum_notes_engraver::Drum_notes_engraver (Context *c)
  : Engraver (c)
{
}

void
Drum_notes_engraver::listen_note (Stream_event *ev)
{
  events_.push_back (ev);
}

void
Drum_notes_engraver::process_music ()
{
  if (events_.empty ())
    return;

  SCM tab = get_property (this, "drumStyleTable");
  for (vsize i = 0; i < events_.size (); i++)
    {
      Stream_event *ev = events_[i];
      Item *note = make_item ("NoteHead", ev->self_scm ());

      SCM drum_type = get_property (ev, "drum-type");

      SCM defn = SCM_EOL;

      if (from_scm<bool> (scm_hash_table_p (tab)))
        defn = scm_hashq_ref (tab, drum_type, SCM_EOL);

      if (scm_is_pair (defn))
        {
          SCM pos = scm_caddr (defn);
          SCM style = scm_car (defn);
          SCM script = scm_cadr (defn);

          if (scm_is_integer (pos))
            set_property (note, "staff-position", pos);
          if (scm_is_symbol (style))
            set_property (note, "style", style);

          if (scm_is_true (script))
            {
              // Error out if script doesn't exist
              if (scm_is_false (ly_assoc (
                    script, get_property (context (), "scriptDefinitions"))))
                ev->origin ()->error (
                  _f ("unrecognised percussion sign: \"%s\"",
                      ly_scm_write_string (script)));

              Item *p = make_item ("Script", ev->self_scm ());
              make_script_from_event (p, context (), script, 0);

              p->set_y_parent (note);
              Side_position_interface::add_support (p, note);
              scripts_.push_back (p);
            }
        }
    }
}

void
Drum_notes_engraver::acknowledge_stem (Grob_info inf)
{
  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Grob *e = scripts_[i];

      if (from_scm<Direction> (get_property (e, "side-relative-direction")))
        set_object (e, "direction-source", inf.grob ()->self_scm ());

      Side_position_interface::add_support (e, inf.grob ());
    }
}

void
Drum_notes_engraver::acknowledge_note_column (Grob_info_t<Item> inf)
{
  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Grob *e = scripts_[i];

      if (!e->get_x_parent () && Side_position_interface::is_on_y_axis (e))
        e->set_x_parent (inf.grob ());

      Side_position_interface::add_support (e, inf.grob ());
    }
}

void
Drum_notes_engraver::stop_translation_timestep ()
{
  scripts_.clear ();
  events_.clear ();
}

void
Drum_notes_engraver::boot ()
{
  ADD_LISTENER (note);
  ADD_ACKNOWLEDGER (stem);
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Drum_notes_engraver,
                /* doc */
                R"(
Generate drum note heads.
                )",

                /* create */
                R"(
NoteHead
Script
                )",

                /* read */
                R"(
drumStyleTable
                )",

                /* write */
                R"(

                )");
