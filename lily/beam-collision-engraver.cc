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
  DECLARE_ACKNOWLEDGER (beam);
  DECLARE_END_ACKNOWLEDGER (beam);
  void stop_translation_timestep ();
public:
  TRANSLATOR_DECLARATIONS (Beam_collision_engraver);
};

void
Beam_collision_engraver::stop_translation_timestep ()
{
  /* 
     First, for all grobs that fall to the left of a beam during
     a timestep (i.e. clefs, time signatures), add these to
     the beams that are currently active.
  */
  for (vsize i = 0; i < covered_interior_grobs_.size (); i++)
    for (vsize j = 0; j < active_beams_.size (); j++)
      Pointer_group_interface::add_grob (active_beams_[j], ly_symbol2scm ("covered-grobs"), covered_interior_grobs_[i]);

  covered_interior_grobs_.clear ();

  /*
     If a signaled beam is already in active_beams_, we erase it so as
     not to have a beam represented in active_beams_ more than once.
   */
  
  for (vsize i = 0; i < active_beams_.size (); i++)
    for (vsize j = 0; j < signaled_beams_.size (); j++)
      if (active_beams_[i] == signaled_beams_[j])
        {
          signaled_beams_.erase (signaled_beams_.begin () + j);
          break;
        }

  /*
     In auto beaming, beams both begin and end during the same timestep.
     This means that if there is a beam that is both in signaled_beams_ and
     end_beams_, it must either be an auto beam (likely) or a beam that
     has no notes under it (highly unlikely).  In either case, we cannot account
     for the grobs under this beam, and we erase it from signaled beams.
  */
  for (vsize i = 0; i < end_beams_.size (); i++)
    for (vsize j = 0; j < signaled_beams_.size (); j++)
      if (end_beams_[i] == signaled_beams_[j])
        {
          signaled_beams_.erase (signaled_beams_.begin () + j);
          break;
        }

  /*
     We want to know how big active beams was originally so that we do not
     get any cyclical dependencies (see below).
  */
  vsize orig_size = active_beams_.size ();

  /*
     All signaled beams that are left now become active beams that are fair
     game to collect covered grobs.
  */
  for (vsize i=0; i < signaled_beams_.size (); i++)
    active_beams_.push_back (signaled_beams_[i]);

  /*
     Add all covered grobs that fall to the right of a beam (like noteheads)
     as to covered-grobs of the beam.  Note that noteheads that part of a beam
     are not added to that list, as note heads should not never collide with
     their own beams due to minimum stem length penalties in beam-quanting.cc.
  */
  for (vsize i = 0; i < covered_grobs_.size (); i++)
    for (vsize j = 0; j < active_beams_.size (); j++)
      {
        bool my_beam = false;
        if (Grob *stem = unsmob_grob (covered_grobs_[i]->get_object ("stem")))
          if (Grob *beam = unsmob_grob (stem->get_object ("beam")))
            if (beam == active_beams_.at (j))
              my_beam = true;
        if (!my_beam)
          Pointer_group_interface::add_grob (active_beams_.at (j), ly_symbol2scm ("covered-grobs"), covered_grobs_[i]);
      }

  covered_grobs_.clear ();

  /*
     This is where cyclical dependencies are avoided.  In beam collision avoidance,
     beams often need to avoid other beams.  To do this, they need to know the beam's
     position.  But, if that second beam needs to know the first beam's position, we
     have a cyclical dependency.  So, we only ever add signaled beams to active_beams_
     that existed BEFORE this time step.  This is controlled by the orig_size variable.
     The for loop stops before it gets to the signaled beams added above so that beams
     added during this timestep are never dependent on each other for positioning.
  */
  for (vsize i = 0; i < signaled_beams_.size (); i++)
    for (vsize j = 0; j < orig_size; j++)
      Pointer_group_interface::add_grob (active_beams_[j], ly_symbol2scm ("covered-grobs"), signaled_beams_[i]);

  signaled_beams_.clear ();

  /*
     If the end of a beam has been announced, it is no longer active.  So, remove this beam
     from active_beams_.
  */
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
