/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "item.hh"
#include "lily-guile.hh"
#include "lily-guile-macros.hh"
#include "rational.hh"
#include "stem.hh"
#include "template-engraver-for-beams.hh"
#include "tuplet-description.hh"

void
Template_engraver_for_beams::derived_mark () const
{
  beaming_options_.gc_mark ();
  finished_beaming_options_.gc_mark ();
}

void
Template_engraver_for_beams::begin_beam ()
{
  beam_start_position_
    = from_scm (get_property (this, "measurePosition"), Moment (0));
  beam_start_moment_ = now_mom ();

  beaming_options_.from_context (context ());
  Rational measure_offset = beam_start_position_.grace_part_
                              ? beam_start_position_.grace_part_
                              : beam_start_position_.main_part_;

  // measure_offset is not allowed to be negative, so modulo
  if (measure_offset < 0)
    {
      Rational const measure_length
        = isfinite (beaming_options_.measure_length_)
            ? beaming_options_.measure_length_
            : Rational (1);
      measure_offset = measure_offset.mod_rat (measure_length);
      if (measure_offset < 0)
        measure_offset += measure_length;
    }
  beam_pattern_ = new Beaming_pattern (measure_offset);
}

void
Template_engraver_for_beams::typeset_beam ()
{
  if (finished_beam_)
    {
      finished_beam_pattern_->beamify (finished_beaming_options_);

      Beam::set_beaming (finished_beam_, finished_beam_pattern_);
      finished_beam_ = nullptr;

      delete finished_beam_pattern_;
      finished_beam_pattern_ = nullptr;
    }
}

void
Template_engraver_for_beams::add_stem (Item *stem, Duration const &dur)
{
  beam_pattern_->add_stem (last_added_moment_.grace_part_
                             ? last_added_moment_.grace_part_
                             : last_added_moment_.main_part_,
                           Stem::is_invisible (stem), dur,
                           unsmob<Tuplet_description> (get_property (
                             context (), "currentTupletDescription")));
}
