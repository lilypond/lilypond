/*
  spacing-basic.cc -- implement Spacing_spanner, simplistic spacing routines

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "spacing-spanner.hh"
#include "moment.hh"
#include "paper-column.hh"
#include "misc.hh"
#include "warn.hh"

/*
  LilyPond spaces by taking a simple-minded spacing algorithm, and
  adding subtle adjustments to that. This file does the simple-minded
  spacing routines.
*/

/*
  Get the measure wide ant for arithmetic spacing.
*/
Real
Spacing_options::get_duration_space (Moment d,
				     bool *expand_only) const
{
  Real k = shortest_duration_space_;

  if (d < global_shortest_)
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
      Rational ratio = d.main_part_ / global_shortest_;

      return ((k - 1) + double (ratio)) * increment_;
    }
  else
    {
      /*
	John S. Gourlay. ``Spacing a Line of Music, '' Technical
	Report OSU-CISRC-10/87-TR35, Department of Computer and
	Information Science, The Ohio State University, 1987.
      */
      Real log = log_2 (global_shortest_);
      k -= log;
      Rational compdur = d.main_part_ + d.grace_part_ / Rational (3);
      *expand_only = false;

      return (log_2 (compdur) + k) * increment_;
    }
}

/*
  The one-size-fits all spacing. It doesn't take into account
  different spacing wishes from one to the next column.
*/
void
Spacing_spanner::standard_breakable_column_spacing (Grob *me, Item *l, Item *r,
						    Real *fixed, Real *space,
						    Spacing_options const *options)
{
  *fixed = 0.0;
  Direction d = LEFT;
  Drul_array<Item *> cols (l, r);

  do
    {
      if (!Paper_column::is_musical (cols[d]))
	{
	  /*
	    Tied accidentals over barlines cause problems, so lets see
	    what happens if we do this for non musical columns only.
	  */
	  Interval lext = cols[d]->extent (cols [d], X_AXIS);
	  if (!lext.is_empty ())
	    *fixed += -d * lext[-d];
	}
    }
  while (flip (&d) != LEFT);

  if (Paper_column::is_breakable (l) && Paper_column::is_breakable (r))
    {
      Moment *dt = unsmob_moment (l->get_property ("measure-length"));
      Moment mlen (1);
      if (dt)
	mlen = *dt;

      Real incr = robust_scm2double (me->get_property ("spacing-increment"), 1);

      *space = *fixed + incr * double (mlen.main_part_ / options->global_shortest_) * 0.8;
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
	  *space = *fixed + 0.5;
	}
      else
	{
	  bool dummy;
	  *space = *fixed + options->get_duration_space (dt, &dummy);
	}
    }
}

Real
Spacing_spanner::note_spacing (Grob *me, Grob *lc, Grob *rc,
			       Spacing_options const *options,
			       bool *expand_only)
{
  Moment shortest_playing_len = 0;
  SCM s = lc->get_property ("shortest-playing-duration");

  if (unsmob_moment (s))
    shortest_playing_len = *unsmob_moment (s);

  if (! shortest_playing_len.to_bool ())
    {
      programming_error ("can't find a ruling note at " + Paper_column::when_mom (lc).to_string ());
      shortest_playing_len = 1;
    }

  Moment lwhen = Paper_column::when_mom (lc);
  Moment rwhen = Paper_column::when_mom (rc);

  Moment delta_t = rwhen - lwhen;
  if (!Paper_column::is_musical (rc))
    {
      /*
	when toying with mmrests, it is possible to have musical
	column on the left and non-musical on the right, spanning
	several measures.
      */

      Moment *dt = unsmob_moment (rc->get_property ("measure-length"));
      if (dt)
	{
	  delta_t = min (delta_t, *dt);

	  /*
	    The following is an extra safety measure, such that
	    the length of a mmrest event doesn't cause havoc.
	  */
	  shortest_playing_len = min (shortest_playing_len, *dt);
	}
    }

  Real dist = 0.0;
  if (delta_t.main_part_ && !lwhen.grace_part_)
    {
      dist = options->get_duration_space (shortest_playing_len,
					  expand_only);
      dist *= double (delta_t.main_part_ / shortest_playing_len.main_part_);
    }
  else if (delta_t.grace_part_)
    {
      /*
	Crude hack for spacing graces: we take the shortest space
	available (namely the space for the global shortest note), and
	multiply that by grace-space-factor
      */
      dist = options->get_duration_space (options->global_shortest_, expand_only);

      Real grace_fact
	= robust_scm2double (me->get_property ("grace-space-factor"), 1);

      dist *= grace_fact;
    }

  return dist;
}

/****************************************************************/

void
Spacing_options::init_from_grob (Grob *me)
{
  increment_ = robust_scm2double (me->get_property ("spacing-increment"), 1);

  packed_ = to_boolean (me->get_property ("packed-spacing"));
  stretch_uniformly_ = to_boolean (me->get_property ("uniform-stretching"));
  float_nonmusical_columns_
    = to_boolean (me->get_property ("strict-note-spacing"));
  shortest_duration_space_ = robust_scm2double (me->get_property ("shortest-duration-space"), 1);
}

void
Spacing_options::init ()
{
  increment_ = 1.2;
  packed_ = false;
  stretch_uniformly_ = false;
  float_nonmusical_columns_ = false;
  shortest_duration_space_ = 2.0;

  global_shortest_ = Rational (1, 8);
}
