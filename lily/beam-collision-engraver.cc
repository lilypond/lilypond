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
  vector<Grob *> beams_;
  vector<Grob *> covered_grobs_;

  DECLARE_ACKNOWLEDGER (note_head);
  DECLARE_ACKNOWLEDGER (accidental);
  DECLARE_ACKNOWLEDGER (clef);
  DECLARE_ACKNOWLEDGER (key_signature);
  DECLARE_ACKNOWLEDGER (time_signature);
  DECLARE_ACKNOWLEDGER (beam);

  virtual void finalize ();

public:
  TRANSLATOR_DECLARATIONS (Beam_collision_engraver);
};

Beam_collision_engraver::Beam_collision_engraver () {}

void
Beam_collision_engraver::finalize ()
{
  if (!covered_grobs_.size ())
    return;

  vector_sort (covered_grobs_, Grob::less);
  vector_sort (beams_, Grob::less);
  vsize start = 0;

  for (vsize i = 0; i < beams_.size (); i++)
    {
      Interval_t<int> beam_spanned_rank_ = beams_[i]->spanned_rank_interval ();
      // Start considering grobs at the first grob whose end falls at or after the beam's beginning.
      while (covered_grobs_[start]->spanned_rank_interval ()[RIGHT] < beam_spanned_rank_[LEFT])
        start++;

      // Stop when the grob's beginning comes after the beam's end.
      for (vsize j = start; j < covered_grobs_.size (); j++)
        {
          Interval_t<int> covered_grob_spanned_rank = covered_grobs_[j]->spanned_rank_interval ();
          if (covered_grob_spanned_rank[LEFT] > beam_spanned_rank_[RIGHT])
            break;
          /*
             Only consider grobs whose end falls at or after the beam's beginning.
             If the grob is a beam, it cannot start before beams_[i]
          */
          if ((covered_grob_spanned_rank[RIGHT] >= beam_spanned_rank_[LEFT])
              && !(Beam::has_interface (covered_grobs_[j])
                   && (covered_grob_spanned_rank[LEFT] <= beam_spanned_rank_[LEFT])))
            {
              // Do not consider note heads attached to the beam.
              bool my_beam = false;
              if (Grob *stem = unsmob_grob (covered_grobs_[j]->get_object ("stem")))
                if (Grob *beam = unsmob_grob (stem->get_object ("beam")))
                  if (beam == beams_[i])
                    my_beam = true;

              if (!my_beam)
                Pointer_group_interface::add_grob (beams_[i], ly_symbol2scm ("covered-grobs"), covered_grobs_[j]);
            }
        }
    }
}

void
Beam_collision_engraver::acknowledge_note_head (Grob_info i)
{
  covered_grobs_.push_back (i.grob ());
}

void
Beam_collision_engraver::acknowledge_accidental (Grob_info i)
{
  if (i.grob ()->internal_has_interface (ly_symbol2scm ("inline-accidental-interface")))
    covered_grobs_.push_back (i.grob ());
}

void
Beam_collision_engraver::acknowledge_clef (Grob_info i)
{
  covered_grobs_.push_back (i.grob ());
}

void
Beam_collision_engraver::acknowledge_key_signature (Grob_info i)
{
  covered_grobs_.push_back (i.grob ());
}

void
Beam_collision_engraver::acknowledge_time_signature (Grob_info i)
{
  covered_grobs_.push_back (i.grob ());
}

void
Beam_collision_engraver::acknowledge_beam (Grob_info i)
{
  beams_.push_back (i.grob ());
  covered_grobs_.push_back (i.grob ());
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Beam_collision_engraver, note_head);
ADD_ACKNOWLEDGER (Beam_collision_engraver, accidental);
ADD_ACKNOWLEDGER (Beam_collision_engraver, clef);
ADD_ACKNOWLEDGER (Beam_collision_engraver, key_signature);
ADD_ACKNOWLEDGER (Beam_collision_engraver, time_signature);
ADD_ACKNOWLEDGER (Beam_collision_engraver, beam);

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
