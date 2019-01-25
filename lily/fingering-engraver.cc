/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2019 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "rhythmic-head.hh"
#include "self-alignment-interface.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "stream-event.hh"
#include "item.hh"

#include "translator.icc"

class Fingering_engraver : public Engraver
{
  vector<Stream_event *> events_;
  vector<Item *> fingerings_;

public:
  TRANSLATOR_DECLARATIONS (Fingering_engraver);
protected:
  void stop_translation_timestep ();
  void process_music ();
  void listen_fingering (Stream_event *);
  void acknowledge_rhythmic_head (Grob_info);
  void acknowledge_stem (Grob_info);
  void acknowledge_flag (Grob_info);
  void acknowledge_note_column (Grob_info);

private:
  void make_script (Direction, Stream_event *, int);
};

void
Fingering_engraver::listen_fingering (Stream_event *ev)
{
  events_.push_back (ev);
}

void
Fingering_engraver::acknowledge_stem (Grob_info inf)
{
  for (vsize i = 0; i < fingerings_.size (); i++)
    Side_position_interface::add_support (fingerings_[i], inf.grob ());
}

void
Fingering_engraver::acknowledge_flag (Grob_info inf)
{
  for (vsize i = 0; i < fingerings_.size (); i++)
    Side_position_interface::add_support (fingerings_[i], inf.grob ());
}

void
Fingering_engraver::acknowledge_rhythmic_head (Grob_info inf)
{
  for (vsize i = 0; i < fingerings_.size (); i++)
    Side_position_interface::add_support (fingerings_[i], inf.grob ());
}

void
Fingering_engraver::acknowledge_note_column (Grob_info inf)
{
  /* set NoteColumn as parent */
  /* and X-align on main noteheads */
  for (vsize i = 0; i < fingerings_.size (); i++)
    {
      Grob *t = fingerings_[i];
      t->set_parent (inf.grob (), X_AXIS);
      t->set_property ("X-align-on-main-noteheads", SCM_BOOL_T);
    }
}

void
Fingering_engraver::process_music ()
{
  for (vsize i = events_.size (); i--;)
    {
      SCM dir = events_[i]->get_property ("direction");
      make_script (to_dir (dir), events_[i], i);
    }
}

void
Fingering_engraver::make_script (Direction d, Stream_event *r, int i)
{
  Item *fingering = make_item ("Fingering", r->self_scm ());

  /*
    We can't fold these definitions into define-grobs since
    fingerings for chords need different settings.
  */
  Side_position_interface::set_axis (fingering, Y_AXIS);
  Self_alignment_interface::set_aligned_on_parent (fingering, X_AXIS);

  // Hmm
  int priority = 200;
  SCM s = fingering->get_property ("script-priority");
  if (scm_is_number (s))
    priority = scm_to_int (s);

  /* See script-engraver.cc */
  priority += i;

  fingering->set_property ("script-priority", scm_from_int (priority));

  if (d)
    fingering->set_property ("direction", scm_from_int (d));
  else if (!is_direction (fingering->get_property_data ("direction")))
    fingering->set_property ("direction", scm_from_int (UP));

  fingerings_.push_back (fingering);
}

void
Fingering_engraver::stop_translation_timestep ()
{
  fingerings_.clear ();
  events_.clear ();
}

Fingering_engraver::Fingering_engraver (Context *c)
  : Engraver (c)
{
}


void
Fingering_engraver::boot ()
{
  ADD_LISTENER (Fingering_engraver, fingering);
  ADD_ACKNOWLEDGER (Fingering_engraver, rhythmic_head);
  ADD_ACKNOWLEDGER (Fingering_engraver, stem);
  ADD_ACKNOWLEDGER (Fingering_engraver, flag);
  ADD_ACKNOWLEDGER (Fingering_engraver, note_column);
}

ADD_TRANSLATOR (Fingering_engraver,
                /* doc */
                "Create fingering scripts.",

                /* create */
                "Fingering ",

                /* read */
                "",

                /* write */
                ""
               );
