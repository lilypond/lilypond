/*
  Determine whether a beam is concave.
*/

#include <math.h>

#include "group-interface.hh"
#include "array.hh"
#include "stem.hh"
#include "beam.hh"
#include "staff-symbol-referencer.hh"

bool
is_concave_single_notes (Array<int> const &positions, Direction beam_dir)
{
  Interval covering;
  covering.add_point (positions[0]);
  covering.add_point (positions.top ());

  bool above = false;
  bool below = false;
  bool concave = false;

  /*
    notes above and below the interval covered by 1st and last note.
  */
  for (int i = 1; i < positions.size () - 1; i++)
    {
      above = above || (positions[i] > covering[UP]);
      below = below || (positions[i] < covering[DOWN]);
    }

  concave = concave || (above && below);
  /*
    A note as close or closer to the beam than begin and end, but the
    note is reached in the opposite direction as the last-first dy
  */
  int dy = positions.top () - positions[0];
  int closest = (beam_dir * positions.top ()) >? (beam_dir * positions[0]);
  for (int i = 2; !concave && i < positions.size () - 1; i++)
    {
      int inner_dy = positions[i] - positions[i - 1];
      if (sign (inner_dy) != sign (dy)
	  && (beam_dir * positions[i] >= closest
	      || beam_dir * positions[i - 1] >= closest))
	concave = true;
    }

  bool all_closer = true;
  for (int i = 1; all_closer && i < positions.size () - 1; i++)
    {
      all_closer = all_closer
	&& (beam_dir * positions[i] > closest);
    }

  concave = concave || all_closer;
  return concave;
}

Real
calc_concaveness (Array<int> const &positions, Direction beam_dir)
{
  Real dy = positions.top () - positions[0];
  Real slope = dy / Real (positions.size () - 1);
  Real concaveness = 0.0;
  for (int i = 1; i < positions.size () - 1; i++)
    {
      Real line_y = slope * i + positions[0];

      concaveness += (beam_dir * (positions[i] - line_y)) >? 0.0;
    }

  concaveness /= positions.size ();

  /*
    Normalize. For dy = 0, the slope ends up as 0 anyway, so the
    scaling of concaveness doesn't matter much.
  */
  if (dy)
    concaveness /= fabs (dy);
  return concaveness;
}

MAKE_SCHEME_CALLBACK (Beam, check_concave, 1);
SCM
Beam::check_concave (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Link_array<Grob> stems
    = extract_grob_array (me, ly_symbol2scm ("stems"));

  if (is_knee (me))
    return SCM_UNSPECIFIED;

  Direction beam_dir = CENTER;
  for (int i = stems.size (); i--;)
    {
      if (Stem::is_invisible (stems[i]))
	stems.del (i);
      else
	{
	  if (Direction dir = Stem::get_direction (stems[i]))
	    beam_dir = dir;
	}
    }

  if (stems.size () <= 2)
    return SCM_UNSPECIFIED;

  Array<int> close_positions;
  Array<int> far_positions;
  for (int i = 0; i < stems.size (); i++)
    {
      /*
	For chords, we take the note head that is closest to the beam.

	Hmmm.. wait, for the beams in the last measure of morgenlied,
	this doesn't look so good. Let's try the heads farthest from
	the beam.

      */
      Interval posns = Stem::head_positions (stems[i]);

      close_positions.push ((int) rint (posns[beam_dir]));
      far_positions.push ((int) rint (posns[-beam_dir]));
    }

  if (is_concave_single_notes (far_positions, beam_dir))
    {
      Drul_array<Real> pos = ly_scm2interval (me->get_property ("positions"));
      Real r = linear_combination (pos, 0.0);

      r /= Staff_symbol_referencer::staff_space (me);
      me->set_property ("positions", ly_interval2scm (Drul_array<Real> (r, r)));
      me->set_property ("least-squares-dy", scm_make_real (0));
    }
  else
    {
      Real concaveness = (calc_concaveness (far_positions, beam_dir)
			  + calc_concaveness (close_positions, beam_dir)) / 2;

      me->set_property ("concaveness", scm_from_double (concaveness));
    }

  return SCM_UNSPECIFIED;
}
