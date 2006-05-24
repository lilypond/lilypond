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

MAKE_SCHEME_CALLBACK (Stem_tremolo, calc_slope, 1)
SCM
Stem_tremolo::calc_slope (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *stem = unsmob_grob (me->get_object ("stem"));
  Spanner *beam = Stem::get_beam (stem);

  if (beam)
    {
      Real dy = 0;
      SCM s = beam->get_property ("quantized-positions");
      if (is_number_pair (s))
	dy = - scm_to_double (scm_car (s)) + scm_to_double (scm_cdr (s));

      Grob *s2 = Beam::last_visible_stem (beam);
      Grob *s1 = Beam::first_visible_stem (beam);
      
      Grob *common = s1->common_refpoint (s2, X_AXIS);
      Real dx = s2->relative_coordinate (common, X_AXIS) -
	s1->relative_coordinate (common, X_AXIS);

      return scm_from_double (dx ? dy / dx : 0);
    }
  else
    /* down stems with flags should have more sloped trems (helps avoid
       flag/stem collisions without making the stem very long) */
    return scm_from_double (
        (Stem::duration_log (stem) >= 3 && get_grob_direction (stem) == DOWN) ?
          0.40 : 0.25);
}

MAKE_SCHEME_CALLBACK (Stem_tremolo, calc_width, 1)
SCM
Stem_tremolo::calc_width (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *stem = unsmob_grob (me->get_object ("stem"));
  Direction stemdir = get_grob_direction (stem);
  bool beam = Stem::get_beam (stem);
  bool flag = Stem::duration_log (stem) >= 3 && !beam;

  /* beamed stems and up-stems with flags have shorter tremolos */
  return scm_from_double (((stemdir == UP && flag) || beam)? 1.0 : 1.5);
}

MAKE_SCHEME_CALLBACK (Stem_tremolo, calc_style, 1)
SCM
Stem_tremolo::calc_style (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *stem = unsmob_grob (me->get_object ("stem"));
  Direction stemdir = get_grob_direction (stem);
  bool beam = Stem::get_beam (stem);
  bool flag = Stem::duration_log (stem) >= 3 && !beam;

  return ly_symbol2scm (((stemdir == UP && flag) || beam) ? "rectangle" : "default");
}

Real
Stem_tremolo::get_beam_translation (Grob *me)
{
  Grob *stem = unsmob_grob (me->get_object ("stem"));
  Spanner *beam = Stem::get_beam (stem);

  return  beam ? Beam::get_beam_translation (beam) : 0.81;
}

/* FIXME: move to Lookup? */
Stencil
Stem_tremolo::rotated_box (Real slope, Real width, Real thick, Real blot)
{
  vector<Offset> pts;
  Offset rot (1, slope);

  thick -= 2*blot;
  width -= 2*blot;
  rot /= sqrt (1 + slope*slope);
  pts.push_back (Offset (0, -thick / 2) * rot);
  pts.push_back (Offset (width, -thick / 2) * rot);
  pts.push_back (Offset (width, thick / 2) * rot);
  pts.push_back (Offset (0, thick / 2) * rot);
  return Lookup::round_filled_polygon (pts, blot);
}

Stencil
Stem_tremolo::raw_stencil (Grob *me, Real slope, Direction stemdir)
{
  Real ss = Staff_symbol_referencer::staff_space (me);
  Real thick = robust_scm2double (me->get_property ("beam-thickness"), 1);
  Real width = robust_scm2double (me->get_property ("beam-width"), 1);
  Real blot = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));
  SCM style = me->get_property ("style");
  if (!scm_is_symbol (style))
    style = ly_symbol2scm ("default");

  width *= ss;
  thick *= ss;

  Stencil a = style == ly_symbol2scm ("rectangle") ?
    rotated_box (slope, width, thick, blot) :
    Lookup::beam (slope, width, thick, blot);
  a.align_to (X_AXIS, CENTER);
  a.align_to (Y_AXIS, CENTER);

  int tremolo_flags = robust_scm2int (me->get_property ("flag-count"), 0);
  if (!tremolo_flags)
    {
      programming_error ("no tremolo flags");

      me->suicide ();
      return Stencil ();
    }

  Real beam_translation = get_beam_translation(me);

  Stencil mol;
  for (int i = 0; i < tremolo_flags; i++)
    {
      Stencil b (a);
      b.translate_axis (beam_translation * i * stemdir * -1, Y_AXIS);
      mol.add_stencil (b);
    }
  return mol;
}



MAKE_SCHEME_CALLBACK (Stem_tremolo, height, 1);
SCM
Stem_tremolo::height (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  /*
    Cannot use the real slope, since it looks at the Beam.
   */
  Stencil s1 (raw_stencil (me, 0.35, UP));

  return ly_interval2scm (s1.extent (Y_AXIS));
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
  bool whole_note = Stem::duration_log (stem) <= 0;
  if (stemdir == 0)
    stemdir = UP;

  Real beam_translation
    = (beam && beam->is_live ())
    ? Beam::get_beam_translation (beam)
    : 0.81;

  /* for a whole note, we position relative to the notehead, so we want the
     stencil aligned on the flag closest to the head */
  Direction stencil_dir = whole_note ? -stemdir : stemdir;
  Stencil mol = raw_stencil (me, robust_scm2double (me->get_property ("slope"),
						    0.25), stencil_dir);

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
    - stemdir * max (beam_count, 1) * beam_translation;

  if (!beam && Stem::duration_log (stem) >= 3)
    {
      end_y -= stemdir * (Stem::duration_log (stem) - 2) * beam_translation;
      if (stemdir == UP)
        end_y -= stemdir * beam_translation * 0.5;
    }
  if (whole_note)
    {
      /* we shouldn't position relative to the end of the stem since the stem
         is invisible */
      vector<int> nhp = Stem::note_head_positions (stem);
      Real note_head = (stemdir == UP ? nhp.back () : nhp[0]) * ss / 2;
      end_y = note_head + stemdir * 1.5;
    }
  mol.translate_axis (end_y, Y_AXIS);

  return mol.smobbed_copy ();
}

ADD_INTERFACE (Stem_tremolo, "stem-tremolo-interface",
	       "A beam slashing a stem to indicate a tremolo.",

	       "beam-thickness "
	       "beam-width "
	       "flag-count "
	       "stem "
               "style "
	       "slope "
	       );
