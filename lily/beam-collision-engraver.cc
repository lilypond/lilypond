/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2022 Mike Solomon <mike@mikesolomon.org>

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
#include "context.hh"
#include "engraver.hh"
#include "item.hh"
#include "note-head.hh"
#include "pointer-group-interface.hh"
#include "stem.hh"

#include <algorithm>
#include <unordered_set>
#include <vector>

class Beam_collision_engraver : public Engraver
{
  struct Grob_with_context
  {
    Grob_with_context (Grob *grob, Context const *context)
      : grob_ (grob),
        context_ (context)
    {
    }

    Grob *grob_;
    Context const *context_;
  };

protected:
  // Keep track of all stored Contexts to mark them from garbage collection.
  std::unordered_set<Context const *> contexts_;
  std::vector<Grob_with_context> beams_;
  std::vector<Grob_with_context> covered_grobs_;

  void derived_mark () const override
  {
    Engraver::derived_mark ();
    for (const auto &context : contexts_)
      {
        scm_gc_mark (context->self_scm ());
      }
  }

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

  Grob_with_context create_grob_with_context (const Grob_info &i)
  {
    Context const *context = i.origin_engraver ()->context ();
    // Remember this Context so it does not get collected.
    contexts_.insert (context);
    return {i.grob (), context};
  }

public:
  TRANSLATOR_DECLARATIONS (Beam_collision_engraver);

  static bool grob_less (const Grob_with_context &a, const Grob_with_context &b)
  {
    return Grob::less (a.grob_, b.grob_);
  }
};

Beam_collision_engraver::Beam_collision_engraver (Context *c)
  : Engraver (c)
{
}

bool
Beam_collision_engraver::covered_grob_has_interface (Grob *covered_grob,
                                                     Grob *beam)
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

  std::sort (covered_grobs_.begin (), covered_grobs_.end (), grob_less);
  std::sort (beams_.begin (), beams_.end (), grob_less);
  vsize start = 0;

  for (vsize i = 0; i < beams_.size (); i++)
    {
      Grob *beam_grob = beams_[i].grob_;

      extract_grob_set (beam_grob, "normal-stems", stems);
      Interval_t<int> vertical_span;
      for (vsize j = 0; j < stems.size (); j++)
        {
          int vag = Grob::get_vertical_axis_group_index (stems[j]);
          if (vag >= 0)
            vertical_span.add_point (vag);
        }
      Context const *beam_context = beams_[i].context_;

      Interval_t<int> beam_spanned_rank
        = beam_grob->spanned_column_rank_interval ();
      // Start considering grobs at the first grob whose
      // end falls at or after the beam's beginning.
      while (covered_grobs_[start].grob_->spanned_column_rank_interval ()[RIGHT]
             < beam_spanned_rank[LEFT])
        {
          start++;
        }

      // Stop when the grob's beginning comes after the beam's end.
      for (vsize j = start; j < covered_grobs_.size (); j++)
        {
          Grob *covered_grob = covered_grobs_[j].grob_;
          int vag = Grob::get_vertical_axis_group_index (covered_grob);
          if (!vertical_span.contains (vag))
            continue;
          Context const *covered_grob_context = covered_grobs_[j].context_;

          Interval_t<int> covered_grob_spanned_rank
            = covered_grob->spanned_column_rank_interval ();

          if (covered_grob_spanned_rank[LEFT] > beam_spanned_rank[RIGHT])
            break;
          /*
             Only consider grobs whose end falls at or after the beam's beginning.
             If the grob is a beam, it cannot start before beams_[i].
             Also, if the user wants to check for collisions only in the beam's voice,
             then make sure the beam and the covered_grob are in the same voice.
          */
          if ((covered_grob_spanned_rank[RIGHT] >= beam_spanned_rank[LEFT])
              && !(from_scm<bool> (
                     get_property (beam_grob, "collision-voice-only"))
                   && (covered_grob_context != beam_context))
              && !(
                has_interface<Beam> (covered_grob)
                && (covered_grob_spanned_rank[LEFT] <= beam_spanned_rank[LEFT]))
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

              Pointer_group_interface::add_grob (
                beam_grob, ly_symbol2scm ("covered-grobs"), covered_grob);
            }
        }
    }
}

void
Beam_collision_engraver::acknowledge_note_head (Grob_info i)
{
  covered_grobs_.push_back (create_grob_with_context (i));
}

void
Beam_collision_engraver::acknowledge_stem (Grob_info i)
{
  covered_grobs_.push_back (create_grob_with_context (i));
}

void
Beam_collision_engraver::acknowledge_accidental (Grob_info i)
{
  if (i.grob ()->internal_has_interface (
        ly_symbol2scm ("inline-accidental-interface")))
    covered_grobs_.push_back (create_grob_with_context (i));
}

void
Beam_collision_engraver::acknowledge_clef (Grob_info i)
{
  covered_grobs_.push_back (create_grob_with_context (i));
}

void
Beam_collision_engraver::acknowledge_key_signature (Grob_info i)
{
  covered_grobs_.push_back (create_grob_with_context (i));
}

void
Beam_collision_engraver::acknowledge_clef_modifier (Grob_info i)
{
  covered_grobs_.push_back (create_grob_with_context (i));
}

void
Beam_collision_engraver::acknowledge_time_signature (Grob_info i)
{
  covered_grobs_.push_back (create_grob_with_context (i));
}

void
Beam_collision_engraver::acknowledge_flag (Grob_info i)
{
  covered_grobs_.push_back (create_grob_with_context (i));
}

void
Beam_collision_engraver::acknowledge_beam (Grob_info i)
{
  Grob_with_context gc = create_grob_with_context (i);
  beams_.push_back (gc);
  covered_grobs_.push_back (gc);
}

#include "translator.icc"

void
Beam_collision_engraver::boot ()
{
  ADD_ACKNOWLEDGER (note_head);
  ADD_ACKNOWLEDGER (stem);
  ADD_ACKNOWLEDGER (accidental);
  ADD_ACKNOWLEDGER (clef);
  ADD_ACKNOWLEDGER (key_signature);
  ADD_ACKNOWLEDGER (time_signature);
  ADD_ACKNOWLEDGER (clef_modifier);
  ADD_ACKNOWLEDGER (flag);
  ADD_ACKNOWLEDGER (beam);
}

ADD_TRANSLATOR (Beam_collision_engraver,
                /* doc */
                R"(
Help beams avoid colliding with notes and clefs in other voices.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
