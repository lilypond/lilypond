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

#include "spacing-spanner.hh"

#include "moment.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "spacing-interface.hh"
#include "spacing-options.hh"
#include "spring.hh"
#include "system.hh"
#include "warn.hh"

/*
  LilyPond spaces by taking a simple-minded spacing algorithm, and
  adding subtle adjustments to that. This file does the simple-minded
  spacing routines.
*/
/*
  The one-size-fits all spacing. It doesn't take into account
  different spacing wishes from one to the next column.
*/
Spring
Spacing_spanner::standard_breakable_column_spacing (
    Grob *me, Item *l, Item *r, Spacing_options const *options)
{
  Real min_dist = std::max (0.0, Paper_column::minimum_distance (l, r));

  if (Paper_column::is_breakable (l) && Paper_column::is_breakable (r))
    {
      Moment *dt = unsmob<Moment> (l->get_property ("measure-length"));
      Moment mlen (1);
      if (dt)
        mlen = *dt;

      Real incr = robust_scm2double (me->get_property ("spacing-increment"), 1);
      Real space
          = incr * double (mlen.main_part_ / options->global_shortest_) * 0.8;
      Spring spring = Spring (min_dist + space, min_dist);

      /*
        By default, the spring will have an inverse_stretch_strength of
        space+min_dist. However, we don't want stretchability to scale with
        min_dist or else an empty first measure on a line (which has a large
        min_dist because of the clef) will stretch much more than an empty
        measure later in the line.
      */
      spring.set_inverse_stretch_strength (space);
      return spring;
    }

  Moment dt = Paper_column::when_mom (r) - Paper_column::when_mom (l);
  Real ideal;

  if (dt == Moment (0, 0))
    {
      /*
        In this case, Staff_spacing should handle the job,
        using dt when it is 0 is silly.
      */
      ideal = min_dist + 0.5;
    }
  else
    ideal = min_dist + options->get_duration_space (dt.main_part_);

  return Spring (ideal, min_dist);
}

static Moment *
get_measure_length (Paper_column *column)
{
  Grob *sys = column->get_parent (X_AXIS);

  extract_grob_set (sys, "columns", cols);

  vsize col_idx = column->get_rank ();

  do
    {
      if (Moment *len
          = unsmob<Moment> (cols[col_idx]->get_property ("measure-length")))
        {
          return len;
        }
    }
  while (col_idx-- != 0);

  return 0;
}

/* Basic spring based on duration alone */
Spring
Spacing_spanner::note_spacing (Grob * /* me */, Paper_column *lc,
                               Paper_column *rc, Spacing_options const *options)
{
  Moment shortest_playing_len = 0;
  SCM s = lc->get_property ("shortest-playing-duration");

  if (unsmob<Moment> (s))
    shortest_playing_len = *unsmob<Moment> (s);

  if (!shortest_playing_len.to_bool ())
    {
      programming_error ("cannot find a ruling note at: "
                         + Paper_column::when_mom (lc).to_string ());
      shortest_playing_len = 1;
    }

  Moment lwhen = Paper_column::when_mom (lc);
  Moment rwhen = Paper_column::when_mom (rc);

  Moment delta_t = rwhen - lwhen;

  /*
    when toying with mmrests, it is possible to have musical
    column on the left and non-musical on the right, spanning
    several measures.

    TODO: efficiency: measure length can be cached, or stored as
    property in paper-column.
  */

  if (Moment *measure_len = get_measure_length (lc))
    {
      delta_t = std::min (delta_t, *measure_len);

      /*
        The following is an extra safety measure, such that
        the length of a mmrest event doesn't cause havoc.
      */
      shortest_playing_len = std::min (shortest_playing_len, *measure_len);
    }

  Spring ret;
  if (delta_t.main_part_ && !lwhen.grace_part_)
    {
      // A spring of length and stiffness based on the controlling duration
      Real len = options->get_duration_space (shortest_playing_len.main_part_);
      Real min = options->increment_; // canonical notehead width

      // The portion of that spring proportional to the time between lc and rc
      Real fraction = (delta_t.main_part_ / shortest_playing_len.main_part_);
      ret = Spring (fraction * len, fraction * min);

      // Stretch proportional to the space between canonical bare noteheads
      ret.set_inverse_stretch_strength (fraction * std::max (0.1, (len - min)));
    }
  else if (delta_t.grace_part_)
    {
      Grob *grace_spacing = unsmob<Grob> (lc->get_object ("grace-spacing"));
      if (grace_spacing)
        {
          Spacing_options grace_opts;
          grace_opts.init_from_grob (grace_spacing);
          Real len = grace_opts.get_duration_space (delta_t.grace_part_);
          Real min = grace_opts.increment_;
          ret = Spring (len, min);
          // Grace notes should not stretch very much
          ret.set_inverse_stretch_strength (grace_opts.increment_ / 2.0);
        }
      else // Fallback to the old grace spacing: half that of the shortest note
        ret = Spring (options->get_duration_space (options->global_shortest_)
                          / 2.0,
                      options->increment_ / 2.0);
    }

  return ret;
}
