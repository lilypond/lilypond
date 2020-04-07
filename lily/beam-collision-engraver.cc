/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2020 Mike Solomon <mike@mikesolomon.org>

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
#include "stem.hh"

using std::vector;

class Beam_collision_engraver : public Engraver
{
protected:
  vector<Grob_info> beams_;
  vector<Grob_info> covered_grobs_;

  void acknowledge_note_head (Grob_info);
  void acknowledge_stem (Grob_info);
  void acknowledge_accidental (Grob_info);
  void acknowledge_clef (Grob_info);
  void acknowledge_clef_modifier (Grob_info);
  void acknowledge_key_signature (Grob_info);
  void acknowledge_time_signature (Grob_info);
  void acknowledge_beam (Grob_info);
  void acknowledge_flag (Grob_info);

  void finalize () override;

private:
  bool covered_grob_has_interface (Grob *covered_grob, Grob *beam);

public:
  TRANSLATOR_DECLARATIONS (Beam_collision_engraver);
};

Beam_collision_engraver::Beam_collision_engraver (Context *c)
  : Engraver (c)
{}

bool
Beam_collision_engraver::covered_grob_has_interface (Grob *covered_grob, Grob *beam)
{
  SCM interfaces = get_property (beam, "collision-interfaces");

  for (SCM l = interfaces; scm_is_pair (l); l = scm_cdr (l))
    {
      if (covered_grob->internal_has_interface (scm_car (l)))
        return true;
    }

  return false;
}

void
Beam_collision_engraver::finalize ()
{
  if (!covered_grobs_.size ())
    return;

  vector_sort (covered_grobs_, Grob_info::less);
  vector_sort (beams_, Grob_info::less);
  vsize start = 0;

  for (vsize i = 0; i < beams_.size (); i++)
    {
      Grob *beam_grob = beams_[i].grob ();

      extract_grob_set (beam_grob, "normal-stems", stems);
      Interval_t<int> vertical_span;
      for (vsize j = 0; j < stems.size (); j++)
        {
          int vag = Grob::get_vertical_axis_group_index (stems[j]);
          if (vag >= 0)
            vertical_span.add_point (vag);
        }
      Context *beam_context = beams_[i].context ();

      Interval_t<int> beam_spanned_rank_ = beam_grob->spanned_rank_interval ();
      // Start considering grobs at the first grob whose end falls at or after the beam's beginning.
      while (covered_grobs_[start].grob ()->spanned_rank_interval ()[RIGHT] < beam_spanned_rank_[LEFT])
        start++;

      // Stop when the grob's beginning comes after the beam's end.
      for (vsize j = start; j < covered_grobs_.size (); j++)
        {
          Grob *covered_grob = covered_grobs_[j].grob ();
          int vag = Grob::get_vertical_axis_group_index (covered_grob);
          if (!vertical_span.contains (vag))
            continue;
          Context *covered_grob_context = covered_grobs_[j].context ();

          Interval_t<int> covered_grob_spanned_rank = covered_grob->spanned_rank_interval ();
          if (covered_grob_spanned_rank[LEFT] > beam_spanned_rank_[RIGHT])
            break;
          /*
             Only consider grobs whose end falls at or after the beam's beginning.
             If the grob is a beam, it cannot start before beams_[i].
             Also, if the user wants to check for collisions only in the beam's voice,
             then make sure the beam and the covered_grob are in the same voice.
          */
          if ((covered_grob_spanned_rank[RIGHT] >= beam_spanned_rank_[LEFT])
              && !(to_boolean (get_property (beam_grob, "collision-voice-only"))
                   && (covered_grob_context != beam_context))
              && !(has_interface<Beam> (covered_grob)
                   && (covered_grob_spanned_rank[LEFT] <= beam_spanned_rank_[LEFT]))
              && covered_grob_has_interface (covered_grob, beam_grob))
            {
              // Do not consider note heads attached to the beam.
              if (has_interface<Stem> (covered_grob))
                if (unsmob<Grob> (get_object (covered_grob, "beam")))
                  continue;

              if (Grob *stem = unsmob<Grob> (get_object (covered_grob, "stem")))
                if (Grob *beam = unsmob<Grob> (get_object (stem, "beam")))
                  if (beam == beam_grob)
                    continue;

              Pointer_group_interface::add_grob (beam_grob, ly_symbol2scm ("covered-grobs"), covered_grob);
            }
        }
    }
}

void
Beam_collision_engraver::acknowledge_note_head (Grob_info i)
{
  covered_grobs_.push_back (i);
}

void
Beam_collision_engraver::acknowledge_stem (Grob_info i)
{
  covered_grobs_.push_back (i);
}

void
Beam_collision_engraver::acknowledge_accidental (Grob_info i)
{
  if (i.grob ()->internal_has_interface (ly_symbol2scm ("inline-accidental-interface")))
    covered_grobs_.push_back (i);
}

void
Beam_collision_engraver::acknowledge_clef (Grob_info i)
{
  covered_grobs_.push_back (i);
}

void
Beam_collision_engraver::acknowledge_key_signature (Grob_info i)
{
  covered_grobs_.push_back (i);
}

void
Beam_collision_engraver::acknowledge_clef_modifier (Grob_info i)
{
  covered_grobs_.push_back (i);
}

void
Beam_collision_engraver::acknowledge_time_signature (Grob_info i)
{
  covered_grobs_.push_back (i);
}

void
Beam_collision_engraver::acknowledge_flag (Grob_info i)
{
  covered_grobs_.push_back (i);
}

void
Beam_collision_engraver::acknowledge_beam (Grob_info i)
{
  beams_.push_back (i);
  covered_grobs_.push_back (i);
}

#include "translator.icc"

void
Beam_collision_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Beam_collision_engraver, note_head);
  ADD_ACKNOWLEDGER (Beam_collision_engraver, stem);
  ADD_ACKNOWLEDGER (Beam_collision_engraver, accidental);
  ADD_ACKNOWLEDGER (Beam_collision_engraver, clef);
  ADD_ACKNOWLEDGER (Beam_collision_engraver, key_signature);
  ADD_ACKNOWLEDGER (Beam_collision_engraver, time_signature);
  ADD_ACKNOWLEDGER (Beam_collision_engraver, clef_modifier);
  ADD_ACKNOWLEDGER (Beam_collision_engraver, flag);
  ADD_ACKNOWLEDGER (Beam_collision_engraver, beam);
}

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
