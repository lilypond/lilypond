/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011 Mike Solomon <mike@apollinemike.com>

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

#include "beam.hh"
#include "engraver.hh"
#include "item.hh"
#include "note-head.hh"
#include "pointer-group-interface.hh"

class Beam_collision_engraver : public Engraver
{
protected:
  vector<Grob *> active_beams_;
  vector<Grob *> signaled_beams_;
  vector<Grob *> end_beams_;
  vector<Grob *> covered_grobs_;
  vector<Grob *> covered_interior_grobs_;

  DECLARE_ACKNOWLEDGER (note_head);
  DECLARE_ACKNOWLEDGER (accidental);
  DECLARE_ACKNOWLEDGER (clef);
  DECLARE_ACKNOWLEDGER (key_signature);
  DECLARE_ACKNOWLEDGER (time_signature);
  DECLARE_ACKNOWLEDGER (bar_line);
  DECLARE_ACKNOWLEDGER (beam);
  DECLARE_END_ACKNOWLEDGER (beam);
  void stop_translation_timestep ();
public:
  TRANSLATOR_DECLARATIONS (Beam_collision_engraver);
};

void
Beam_collision_engraver::stop_translation_timestep ()
{
  for (vsize i = 0; i < covered_interior_grobs_.size (); i++)
    for (vsize j = 0; j < active_beams_.size (); j++)
      Pointer_group_interface::add_grob (active_beams_[j], ly_symbol2scm ("covered-grobs"), covered_interior_grobs_[i]);

  covered_interior_grobs_.clear ();

  for (vsize i = 0; i < active_beams_.size (); i++)
    for (vsize j = 0; j < signaled_beams_.size (); j++)
      if (active_beams_[i] == signaled_beams_[j])
        {
          signaled_beams_.erase (signaled_beams_.begin () + j);
          break;
        }

  /*
    hack.
    in auto beaming, end beams are signaled with their beams at a later timestep.
    we need to scrub these.
  */
  for (vsize i = 0; i < end_beams_.size (); i++)
    for (vsize j = 0; j < signaled_beams_.size (); j++)
      if (end_beams_[i] == signaled_beams_[j])
        {
          signaled_beams_.erase (signaled_beams_.begin () + j);
          break;
        }

  for (vsize i = 0; i < signaled_beams_.size (); i++)
    active_beams_.push_back (signaled_beams_[i]);

  signaled_beams_.clear ();

  for (vsize i = 0; i < covered_grobs_.size (); i++)
    for (vsize j = 0; j < active_beams_.size (); j++)
      {
        Grob *g = covered_grobs_[i];
        if (Grob *stem = unsmob_grob (g->get_object ("stem")))
          if (Grob *beam = unsmob_grob (stem->get_object ("beam")))
            if (beam == active_beams_[j])
              continue;

        Pointer_group_interface::add_grob (active_beams_[j], ly_symbol2scm ("covered-grobs"), g);
      }

  covered_grobs_.clear ();

  for (vsize i = 0; i < end_beams_.size (); i++)
    for (vsize j = 0; j < active_beams_.size (); j++)
      if (end_beams_[i] == active_beams_[j])
        {
          active_beams_.erase (active_beams_.begin () + j);
          break;
        }

  end_beams_.clear ();
}

Beam_collision_engraver::Beam_collision_engraver () {}

void
Beam_collision_engraver::acknowledge_note_head (Grob_info i)
{
  covered_grobs_.push_back (i.grob ());
}

void
Beam_collision_engraver::acknowledge_accidental (Grob_info i)
{
  covered_grobs_.push_back (i.grob ());
}

void
Beam_collision_engraver::acknowledge_bar_line (Grob_info i)
{
  covered_interior_grobs_.push_back (i.grob ());
}

void
Beam_collision_engraver::acknowledge_clef (Grob_info i)
{
  covered_interior_grobs_.push_back (i.grob ());
}

void
Beam_collision_engraver::acknowledge_key_signature (Grob_info i)
{
  covered_interior_grobs_.push_back (i.grob ());
}

void
Beam_collision_engraver::acknowledge_time_signature (Grob_info i)
{
  covered_interior_grobs_.push_back (i.grob ());
}

void
Beam_collision_engraver::acknowledge_beam (Grob_info i)
{
  signaled_beams_.push_back (i.grob ());
}

void
Beam_collision_engraver::acknowledge_end_beam (Grob_info i)
{
  end_beams_.push_back (i.grob ());
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Beam_collision_engraver, note_head);
ADD_ACKNOWLEDGER (Beam_collision_engraver, accidental);
ADD_ACKNOWLEDGER (Beam_collision_engraver, clef);
ADD_ACKNOWLEDGER (Beam_collision_engraver, key_signature);
ADD_ACKNOWLEDGER (Beam_collision_engraver, time_signature);
ADD_ACKNOWLEDGER (Beam_collision_engraver, beam);
ADD_ACKNOWLEDGER (Beam_collision_engraver, bar_line);
ADD_END_ACKNOWLEDGER (Beam_collision_engraver, beam);

ADD_TRANSLATOR (Beam_collision_engraver,
		/* doc */
		"Help beams avoid colliding with notes and clefs in other voices.",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);
