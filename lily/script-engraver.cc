/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

using std::vector;

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
  vector<Script_tuple> scripts_;

protected:
  void stop_translation_timestep ();
  void process_music ();

  void listen_articulation (Stream_event *);
  void acknowledge_rhythmic_head (Grob_info);
  void acknowledge_stem (Grob_info);
  void acknowledge_stem_tremolo (Grob_info);
  void acknowledge_tie (Grob_info);
  void acknowledge_end_tie (Grob_info);
  void acknowledge_note_column (Grob_info);
  void acknowledge_inline_accidental (Grob_info);

public:
  TRANSLATOR_DECLARATIONS (Script_engraver);
};

Script_engraver::Script_engraver (Context *c) : Engraver (c) {}

void
Script_engraver::listen_articulation (Stream_event *ev)
{
  /* Discard double articulations for part-combining.  */
  for (vsize i = 0; i < scripts_.size (); i++)
    if (ly_is_equal (scripts_[i].event_->get_property ("articulation-type"),
                     ev->get_property ("articulation-type")))
      return;

  Script_tuple t;
  t.event_ = ev;
  scripts_.push_back (t);
}

void
copy_property (Grob *g, SCM sym, SCM alist)
{
  if (scm_is_null (g->get_property (sym)))
    {
      SCM entry = scm_assoc (sym, alist);
      if (scm_is_pair (entry))
        g->set_property (sym, scm_cdr (entry));
    }
}

/* Add the properties, one by one for each Script.  A little memory
   could be saved by tacking the props onto the Script grob (i.e. make
   ScriptStaccato , ScriptMarcato, etc. ).
*/
void
make_script_from_event (Grob *p, Context *tg, SCM art_type, int index)
{
  SCM alist = tg->get_property ("scriptDefinitions");
  SCM art = scm_assoc (art_type, alist);

  if (scm_is_false (art))
    {
      /* FIXME: */
      warning (_ ("do not know how to interpret articulation:"));
      warning (_ (" scheme encoding: "));
      scm_write (art_type, scm_current_error_port ());
      message ("");
      return;
    }

  art = scm_cdr (art);

  bool priority_found = false;

  for (SCM s = art; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM sym = scm_caar (s);
      SCM type = scm_object_property (sym, ly_symbol2scm ("backend-type?"));
      if (!ly_is_procedure (type))
        continue;

      SCM val = scm_cdar (s);

      if (scm_is_eq (sym, ly_symbol2scm ("script-priority")))
        {
          priority_found = true;
          /* Make sure they're in order of user input by adding index i.
             Don't use the direction in this priority. Smaller means closer
             to the head.  */
          int prio = scm_to_int (val) + index;

          val = scm_from_int (prio);
        }

      SCM preset = p->get_property_data (sym);
      if (scm_is_null (val) || scm_is_false (scm_call_1 (type, preset)))
        p->set_property (sym, val);
    }

  if (!priority_found)
    {
      p->set_property ("script-priority", scm_from_int (index));
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
                              ev->get_property ("articulation-type"), i);

      scripts_[i].script_ = p;

      SCM force_dir = ev->get_property ("direction");
      if (is_direction (force_dir) && to_dir (force_dir))
        p->set_property ("direction", force_dir);
    }
}

void
Script_engraver::acknowledge_stem (Grob_info info)
{
  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Grob *e = scripts_[i].script_;

      if (to_dir (e->get_property ("side-relative-direction")))
        e->set_object ("direction-source", info.grob ()->self_scm ());

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

          if (Side_position_interface::get_axis (e) == X_AXIS
              && !e->get_parent (Y_AXIS))
            {
              e->set_parent (info.grob (), Y_AXIS);
            }
          Side_position_interface::add_support (e, info.grob ());
        }
    }
}

void
Script_engraver::acknowledge_note_column (Grob_info info)
{
  /* Make note column the parent of the script.  That is not
     correct, but due to seconds in a chord, noteheads may be
     swapped around horizontally.

     As the note head to put it on is not known now, postpone this
     decision to Script_interface::calc_direction ().  */
  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Grob *e = scripts_[i].script_;

      if (!e->get_parent (X_AXIS)
          && Side_position_interface::get_axis (e) == Y_AXIS)
        e->set_parent (info.grob (), X_AXIS);
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
  ADD_LISTENER (Script_engraver, articulation);
  ADD_ACKNOWLEDGER (Script_engraver, rhythmic_head);
  ADD_ACKNOWLEDGER (Script_engraver, stem);
  ADD_ACKNOWLEDGER (Script_engraver, tie);
  ADD_END_ACKNOWLEDGER (Script_engraver, tie);
  ADD_ACKNOWLEDGER (Script_engraver, note_column);
  ADD_ACKNOWLEDGER (Script_engraver, stem_tremolo);
  ADD_ACKNOWLEDGER (Script_engraver, inline_accidental);
}

ADD_TRANSLATOR (Script_engraver,
                /* doc */
                "Handle note scripted articulations.",

                /* create */
                "Script ",

                /* read */
                "scriptDefinitions ",

                /* write */
                "");
