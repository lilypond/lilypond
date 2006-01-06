/*
  stem-tremolo.cc -- implement Stem_tremolo

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "stem-tremolo.hh"

#include "spanner.hh"
#include "beam.hh"
#include "directional-element-interface.hh"
#include "item.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "warn.hh"

/* TODO: lengthen stem if necessary  */

MAKE_SCHEME_CALLBACK (Stem_tremolo, dim_callback, 1);

/* todo: init with cons.  */
SCM
Stem_tremolo::dim_callback (SCM e)
{
  Grob *se = unsmob_grob (e);

  Real space = Staff_symbol_referencer::staff_space (se);
  return ly_interval2scm (Interval (-space, space));
}

MAKE_SCHEME_CALLBACK (Stem_tremolo, height, 1);
SCM
Stem_tremolo::height (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  /* TODO: fixme. uncached? */
  return ly_interval2scm (me->get_stencil ()
			  ? me->get_stencil ()->extent (Y_AXIS)
			  : Interval());
}

Stencil
Stem_tremolo::raw_stencil (Grob *me)
{
  Grob *stem = unsmob_grob (me->get_object ("stem"));
  Spanner *beam = Stem::get_beam (stem);

  SCM slope = me->get_property ("slope");
  Real dydx = 0.25;
  if (scm_is_number (slope))
    {
      dydx = robust_scm2double (slope, 0.0);
    }
  else
    {
      if (beam)
	{
	  Real dy = 0;
	  SCM s = beam->get_property ("positions");
	  if (is_number_pair (s))
	    dy = - scm_to_double (scm_car (s)) + scm_to_double (scm_cdr (s));

	  Real dx = Beam::last_visible_stem (beam)->relative_coordinate (0, X_AXIS)
	    - Beam::first_visible_stem (beam)->relative_coordinate (0, X_AXIS);
	  dydx = dx ? dy / dx : 0;
	}
    }
  Real ss = Staff_symbol_referencer::staff_space (me);
  Real thick = robust_scm2double (me->get_property ("beam-thickness"), 1);
  Real width = robust_scm2double (me->get_property ("beam-width"), 1);
  Real blot = me->layout ()->get_dimension (ly_symbol2scm ("blotdiameter"));

  width *= ss;
  thick *= ss;

  Stencil a (Lookup::beam (dydx, width, thick, blot));
  a.translate (Offset (-width * 0.5, width * 0.5 * dydx));

  int tremolo_flags = 0;
  SCM s = me->get_property ("flag-count");
  if (scm_is_number (s))
    tremolo_flags = scm_to_int (s);

  if (!tremolo_flags)
    {
      programming_error ("no tremolo flags");

      me->suicide ();
      return Stencil ();
    }

  /* Who the fuck is 0.81 ? --hwn.   */
  Real beam_translation = beam ? Beam::get_beam_translation (beam) : 0.81;

  Stencil mol;
  for (int i = 0; i < tremolo_flags; i++)
    {
      Stencil b (a);
      b.translate_axis (beam_translation * i, Y_AXIS);
      mol.add_stencil (b);
    }
  return mol;
}

MAKE_SCHEME_CALLBACK (Stem_tremolo, print, 1);
SCM
Stem_tremolo::print (SCM grob)
{
  Grob *me = unsmob_grob (grob);
  Grob *stem = unsmob_grob (me->get_object ("stem"));
  if (!stem)
    {
      programming_error ("no stem for stem-tremolo");
      return SCM_EOL;
    }

  Spanner *beam = Stem::get_beam (stem);
  Direction stemdir = get_grob_direction (stem);
  if (stemdir == 0)
    stemdir = UP;

  Real beam_translation
    = (beam && beam->is_live ())
    ? Beam::get_beam_translation (beam)
    : 0.81;

  Stencil mol = raw_stencil (me);
  Interval mol_ext = mol.extent (Y_AXIS);
  Real ss = Staff_symbol_referencer::staff_space (me);

  // ugh, rather calc from Stem_tremolo_req
  int beam_count = beam ? (Stem::beam_multiplicity (stem).length () + 1) : 0;

  Real beamthickness = 0.0;
  SCM sbt = (beam) ? beam->get_property ("thickness") : SCM_EOL;
  if (scm_is_number (sbt))
    beamthickness = scm_to_double (sbt) * ss;

  Real end_y
    = Stem::stem_end_position (stem) * ss / 2
    - stemdir * (beam_count * beamthickness
		 + (max (beam_count -1, 0) * beam_translation));

  /* FIXME: the 0.33 ss is to compensate for the size of the note head.  */
  Real chord_start_y = Stem::chord_start_y (stem) + 0.33 * ss * stemdir;

  Real padding = beam_translation;

  /* if there is a flag, just above/below the notehead.
     if there is not enough space, center on remaining space,
     else one beamspace away from stem end.  */
  if (!beam && Stem::duration_log (stem) >= 3)
    {
      mol.align_to (Y_AXIS, -stemdir);
      mol.translate_axis (chord_start_y + 0.5 * stemdir, Y_AXIS);
    }
  else if (stemdir * (end_y - chord_start_y) - 2 * padding - mol_ext.length ()
	   < 0.0)
    mol.translate_axis (0.5 * (end_y + chord_start_y) - mol_ext.center (),
			Y_AXIS);
  else
    mol.translate_axis (end_y - stemdir * beam_translation -mol_ext [stemdir],
			Y_AXIS);

  return mol.smobbed_copy ();
}

ADD_INTERFACE (Stem_tremolo, "stem-tremolo-interface",
	       "A beam slashing a stem to indicate a tremolo.",
	       "stem "
	       "slope "
	       "beam-width "
	       "beam-thickness "
	       "flag-count");
