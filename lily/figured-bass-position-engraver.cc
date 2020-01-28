/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>


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

#include "axis-group-interface.hh"
#include "context.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "spanner.hh"
#include "translator.icc"

using std::vector;

class Figured_bass_position_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Figured_bass_position_engraver);

  Spanner *bass_figure_alignment_;
  Spanner *positioner_;
  vector<Grob *> support_;
  vector<Grob *> span_support_;

protected:
  void acknowledge_note_column (Grob_info);
  void acknowledge_slur (Grob_info);
  void acknowledge_end_slur (Grob_info);
  void acknowledge_end_tie (Grob_info);
  void acknowledge_bass_figure_alignment (Grob_info);
  void acknowledge_end_bass_figure_alignment (Grob_info);

  void finalize () override;
  void start_spanner ();
  void stop_spanner ();
  void stop_translation_timestep ();
};

Figured_bass_position_engraver::Figured_bass_position_engraver (Context *c)
    : Engraver (c)
{
  positioner_ = 0;
  bass_figure_alignment_ = 0;
}

void
Figured_bass_position_engraver::start_spanner ()
{
  assert (!positioner_);

  positioner_ = make_spanner ("BassFigureAlignmentPositioning",
                              bass_figure_alignment_->self_scm ());
  positioner_->set_bound (LEFT, bass_figure_alignment_->get_bound (LEFT));
  Axis_group_interface::add_element (positioner_, bass_figure_alignment_);
}

void
Figured_bass_position_engraver::stop_spanner ()
{
  if (positioner_ && !positioner_->get_bound (RIGHT))
    {
      positioner_->set_bound (RIGHT, bass_figure_alignment_->get_bound (RIGHT));
    }

  positioner_ = 0;
  bass_figure_alignment_ = 0;
}

void
Figured_bass_position_engraver::finalize ()
{
  stop_spanner ();
}

void
Figured_bass_position_engraver::acknowledge_note_column (Grob_info info)
{
  support_.push_back (info.grob ());
}

void
Figured_bass_position_engraver::acknowledge_end_slur (Grob_info info)
{
  vector<Grob *>::iterator i
      = find (span_support_.begin (), span_support_.end (), info.grob ());

  if (i < span_support_.end ())
    span_support_.erase (i);
}

void
Figured_bass_position_engraver::acknowledge_slur (Grob_info info)
{
  span_support_.push_back (info.grob ());
}

void
Figured_bass_position_engraver::acknowledge_end_tie (Grob_info info)
{
  support_.push_back (info.grob ());
}

void
Figured_bass_position_engraver::stop_translation_timestep ()
{
  if (positioner_)
    {
      for (vsize i = 0; i < span_support_.size (); i++)
        Side_position_interface::add_support (positioner_, span_support_[i]);
      for (vsize i = 0; i < support_.size (); i++)
        Side_position_interface::add_support (positioner_, support_[i]);
    }

  support_.clear ();
}

void Figured_bass_position_engraver::acknowledge_end_bass_figure_alignment (
    Grob_info /* info */)
{
  stop_spanner ();
}

void
Figured_bass_position_engraver::acknowledge_bass_figure_alignment (
    Grob_info info)
{
  bass_figure_alignment_ = dynamic_cast<Spanner *> (info.grob ());
  start_spanner ();
}

void
Figured_bass_position_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Figured_bass_position_engraver, note_column);
  ADD_ACKNOWLEDGER (Figured_bass_position_engraver, slur);
  ADD_END_ACKNOWLEDGER (Figured_bass_position_engraver, slur);
  ADD_END_ACKNOWLEDGER (Figured_bass_position_engraver, tie);
  ADD_ACKNOWLEDGER (Figured_bass_position_engraver, bass_figure_alignment);
  ADD_END_ACKNOWLEDGER (Figured_bass_position_engraver, bass_figure_alignment);
}

ADD_TRANSLATOR (Figured_bass_position_engraver,
                /* doc */
                "Position figured bass alignments over notes.",

                /* create */
                "BassFigureAlignmentPositioning ",

                /* read */
                "",

                /* write */
                "");
