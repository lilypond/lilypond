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

#include "engraver.hh"

#include "context.hh"
#include "directional-element-interface.hh"
#include "international.hh"
#include "item.hh"
#include "ly-scm-list.hh"
#include "note-column.hh"
#include "paper-column.hh"
#include "rhythmic-head.hh"
#include "script-interface.hh"
#include "side-position-interface.hh"
#include "slur.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

#include <string>
#include <vector>

struct Script_tuple
{
  Stream_event *event_;
  Grob *script_;
  Script_tuple ()
  {
    event_ = 0;
    script_ = 0;
  }
};

class Script_engraver : public Engraver
{
  std::vector<Script_tuple> scripts_;

protected:
  void stop_translation_timestep ();
  void process_music ();

  void listen_articulation (Stream_event *);
  void acknowledge_rhythmic_head (Grob_info);
  void acknowledge_stem (Grob_info);
  void acknowledge_stem_tremolo (Grob_info);
  void acknowledge_tie (Grob_info);
  void acknowledge_end_tie (Grob_info);
  void acknowledge_note_column (Grob_info_t<Item>);
  void acknowledge_inline_accidental (Grob_info);

public:
  TRANSLATOR_DECLARATIONS (Script_engraver);
};

Script_engraver::Script_engraver (Context *c)
  : Engraver (c)
{
}

void
Script_engraver::listen_articulation (Stream_event *ev)
{
  /* Discard double articulations for part-combining.  */
  for (vsize i = 0; i < scripts_.size (); i++)
    if (scm_is_eq (get_property (scripts_[i].event_, "articulation-type"),
                   get_property (ev, "articulation-type")))
      return;

  Script_tuple t;
  t.event_ = ev;
  scripts_.push_back (t);
}

/* Add the properties, one by one for each Script.  A little memory
   could be saved by tacking the props onto the Script grob (i.e. make
   ScriptStaccato , ScriptMarcato, etc. ).
*/
void
make_script_from_event (Grob *p, Context *tg, SCM art_type, size_t index)
{
  // Breathing_sign has similar (but simpler) code.  A change here might
  // warrant a change there.

  if (!scm_is_symbol (art_type))
    {
      std::string msg ("articulation-type must be a symbol since 2.23.6: ");
      msg += ly_scm_write_string (art_type);
      p->programming_error (msg);
    }

  SCM alist = get_property (tg, "scriptDefinitions");
  SCM art = scm_assq (art_type, alist);

  if (scm_is_false (art))
    {
      p->warning (_f ("do not know how to interpret articulation: %s",
                      ly_scm_write_string (art_type)));
      return;
    }

  art = scm_cdr (art);

  bool priority_found = false;

  for (SCM prop_pair : as_ly_scm_list (art))
    {
      SCM sym = scm_car (prop_pair);
      SCM type = scm_object_property (sym, ly_symbol2scm ("backend-type?"));
      if (!ly_is_procedure (type))
        {
          std::string msg ("invalid grob property name in script definition: ");
          msg += ly_scm_write_string (sym);
          p->programming_error (msg);
          continue;
        }

      SCM val = scm_cdr (prop_pair);

      if (scm_is_eq (sym, ly_symbol2scm ("script-priority")))
        {
          priority_found = true;
          /* Make sure they're in order of user input by adding index i.
             Don't use the direction in this priority. Smaller means closer
             to the head.  */
          val = scm_sum (val, to_scm (index));
        }

      SCM preset = get_property_data (p, sym);
      if (scm_is_null (val) || scm_is_false (ly_call (type, preset)))
        set_property (p, sym, val);
    }

  if (!priority_found)
    {
      set_property (p, "script-priority", to_scm (index));
    }
}

void
Script_engraver::process_music ()
{
  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Stream_event *ev = scripts_[i].event_;

      Grob *p = make_item ("Script", ev->self_scm ());

      make_script_from_event (p, context (),
                              get_property (ev, "articulation-type"), i);

      scripts_[i].script_ = p;

      SCM force_dir = get_property (ev, "direction");
      if (is_scm<Direction> (force_dir) && from_scm<Direction> (force_dir))
        set_property (p, "direction", force_dir);
    }
}

void
Script_engraver::acknowledge_stem (Grob_info info)
{
  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Grob *e = scripts_[i].script_;

      if (from_scm<Direction> (get_property (e, "side-relative-direction")))
        set_object (e, "direction-source", info.grob ()->self_scm ());

      Side_position_interface::add_support (e, info.grob ());
    }
}

void
Script_engraver::acknowledge_stem_tremolo (Grob_info info)
{
  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Grob *e = scripts_[i].script_;
      Side_position_interface::add_support (e, info.grob ());
    }
}

void
Script_engraver::acknowledge_tie (Grob_info info)
{
  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Grob *e = scripts_[i].script_;
      Side_position_interface::add_support (e, info.grob ());
    }
}

void
Script_engraver::acknowledge_end_tie (Grob_info info)
{
  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Grob *e = scripts_[i].script_;
      Side_position_interface::add_support (e, info.grob ());
    }
}

void
Script_engraver::acknowledge_inline_accidental (Grob_info info)
{
  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Grob *e = scripts_[i].script_;
      Side_position_interface::add_support (e, info.grob ());
    }
}

void
Script_engraver::acknowledge_rhythmic_head (Grob_info info)
{
  if (info.event_cause ())
    {
      for (vsize i = 0; i < scripts_.size (); i++)
        {
          Grob *e = scripts_[i].script_;

          if (Side_position_interface::is_on_x_axis (e) && !e->get_y_parent ())
            {
              e->set_y_parent (info.grob ());
            }
          Side_position_interface::add_support (e, info.grob ());
        }
    }
}

void
Script_engraver::acknowledge_note_column (Grob_info_t<Item> info)
{
  /* Make note column the parent of the script.  That is not
     correct, but due to seconds in a chord, noteheads may be
     swapped around horizontally.

     As the note head to put it on is not known now, postpone this
     decision to Script_interface::calc_direction ().  */
  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Grob *e = scripts_[i].script_;

      if (!e->get_x_parent () && Side_position_interface::is_on_y_axis (e))
        e->set_x_parent (info.grob ());
    }
}

void
Script_engraver::stop_translation_timestep ()
{
  scripts_.clear ();
}

void
Script_engraver::boot ()
{
  ADD_LISTENER (articulation);
  ADD_ACKNOWLEDGER (rhythmic_head);
  ADD_ACKNOWLEDGER (stem);
  ADD_ACKNOWLEDGER (tie);
  ADD_END_ACKNOWLEDGER (tie);
  ADD_ACKNOWLEDGER (note_column);
  ADD_ACKNOWLEDGER (stem_tremolo);
  ADD_ACKNOWLEDGER (inline_accidental);
}

ADD_TRANSLATOR (Script_engraver,
                /* doc */
                R"(
Handle note scripted articulations.
                )",

                /* create */
                R"(
Script
                )",

                /* read */
                R"(
scriptDefinitions
                )",

                /* write */
                R"(

                )");
