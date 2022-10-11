/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@lilypond.org>


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

#include "spacing-options.hh"
#include "spacing-spanner.hh"
#include "grob.hh"
#include "misc.hh"
#include "moment.hh"
#include "spanner.hh"

void
Spacing_options::init_from_grob (Grob *me)
{
  increment_ = from_scm<double> (get_property (me, "spacing-increment"), 1);

  packed_ = from_scm<bool> (get_property (me, "packed-spacing"));
  stretch_uniformly_ = from_scm<bool> (get_property (me, "uniform-stretching"));
  float_nonmusical_columns_
    = from_scm<bool> (get_property (me, "strict-note-spacing"));
  float_grace_columns_
    = from_scm<bool> (get_property (me, "strict-grace-spacing"));
  shortest_duration_space_
    = from_scm<double> (get_property (me, "shortest-duration-space"), 1);

  const auto shortest_dur
    = from_scm (get_property (me, "common-shortest-duration"),
                Moment (Rational (1, 8), Rational (1, 16)));

  if (shortest_dur.main_part_)
    global_shortest_ = shortest_dur.main_part_;
  else
    global_shortest_ = shortest_dur.grace_part_;
}

Spacing_options::Spacing_options ()
{
  packed_ = false;
  stretch_uniformly_ = false;
  float_nonmusical_columns_ = false;
  float_grace_columns_ = false;

  shortest_duration_space_ = 2.0;
  increment_ = 1.2;

  global_shortest_ = Rational (1, 8);
}

/*
  Get the measure wide ant for arithmetic spacing.
*/
Real
Spacing_options::get_duration_space (Rational d) const
{
  auto ratio = static_cast<Real> (d / global_shortest_);

  if (ratio < 1.0)
    {
      /*
        We don't space really short notes using the log of the
        duration, since it would disproportionally stretches the long
        notes in a piece. In stead, we use geometric spacing with constant 0.5
        (i.e. linear.)

        This should probably be tunable, to use other base numbers.

        In Mozart hrn3 by EB., we have 8th note = 3.9 mm (total), 16th note =
        3.6 mm (total).  head-width = 2.4, so we 1.2mm for 16th, 1.5
        mm for 8th. (white space), suggesting that we use

        (1.2 / 1.5)^{-log2(duration ratio)}


      */

      return (shortest_duration_space_ + ratio - 1) * increment_;
    }
  else
    {
      /*
        John S. Gourlay. ``Spacing a Line of Music, '' Technical
        Report OSU-CISRC-10/87-TR35, Department of Computer and
        Information Science, The Ohio State University, 1987.
      */

      return (shortest_duration_space_ + log_2 (ratio)) * increment_;
    }
}
