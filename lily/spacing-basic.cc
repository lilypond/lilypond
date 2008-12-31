/*
  spacing-basic.cc -- implement Spacing_spanner, simplistic spacing routines

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "spacing-spanner.hh"

#include "spacing-options.hh"
#include "moment.hh"
#include "paper-column.hh"
#include "warn.hh"
#include "pointer-group-interface.hh"
#include "system.hh"
#include "spacing-interface.hh"
#include "spring.hh"

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
Spacing_spanner::standard_breakable_column_spacing (Grob *me, Item *l, Item *r, Spacing_options const *options)
{
  Real min_dist = max (0.0, Paper_column::minimum_distance (l, r));
  Real ideal;

  if (Paper_column::is_breakable (l) && Paper_column::is_breakable (r))
    {
      Moment *dt = unsmob_moment (l->get_property ("measure-length"));
      Moment mlen (1);
      if (dt)
	mlen = *dt;

      Real incr = robust_scm2double (me->get_property ("spacing-increment"), 1);

      ideal = min_dist + incr * double (mlen.main_part_ / options->global_shortest_) * 0.8;
    }
  else
    {
      Moment dt = Paper_column::when_mom (r) - Paper_column::when_mom (l);

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
    }
  return Spring (ideal, min_dist);
}

Moment *
get_measure_length (Grob *column)
{
  Grob * sys = column->get_parent (X_AXIS);

  extract_grob_set (sys, "columns", cols);

  vsize col_idx = Paper_column::get_rank (column);
  
  do
    {
      if (Moment *len = unsmob_moment (cols[col_idx]->get_property ("measure-length")))
	{
	  return len;
	}
    }
  while (col_idx-- != 0);
  
  return 0;
}

Real
Spacing_spanner::note_spacing (Grob * /* me */,
			       Grob *lc,
			       Grob *rc,
			       Spacing_options const *options)
{
  Moment shortest_playing_len = 0;
  SCM s = lc->get_property ("shortest-playing-duration");

  if (unsmob_moment (s))
    shortest_playing_len = *unsmob_moment (s);

  if (! shortest_playing_len.to_bool ())
    {
      programming_error ("cannot find a ruling note at: " + Paper_column::when_mom (lc).to_string ());
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
      delta_t = min (delta_t, *measure_len);

      /*
	The following is an extra safety measure, such that
	the length of a mmrest event doesn't cause havoc.
      */
      shortest_playing_len = min (shortest_playing_len, *measure_len);
    }

  Real dist = 0.0;
  if (delta_t.main_part_ && !lwhen.grace_part_)
    {
      dist = options->get_duration_space (shortest_playing_len.main_part_);
      dist *= double (delta_t.main_part_ / shortest_playing_len.main_part_);
    }
  else if (delta_t.grace_part_)
    {
      /*
	Crude hack for spacing graces: we take the shortest space
	available (namely the space for the global shortest note), and
	multiply that by grace-space-factor
      */
      dist = options->get_duration_space (options->global_shortest_) / 2.0;
      Grob *grace_spacing = unsmob_grob (lc->get_object ("grace-spacing"));
      if (grace_spacing)
	{
	  Spacing_options grace_opts;
	  grace_opts.init_from_grob (grace_spacing);
	  dist = grace_opts.get_duration_space (delta_t.grace_part_);
	}
      
    }

  return dist;
}

