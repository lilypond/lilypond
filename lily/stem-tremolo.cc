/*   
  stem-tremolo.cc --  implement Stem_tremolo
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "stem-tremolo.hh"
#include "warn.hh"
#include "beam.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "stem.hh"
#include "item.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"

/*
  TODO:
    lengthen stem if necessary
 */
MAKE_SCHEME_CALLBACK (Stem_tremolo,dim_callback,2);

/*
  todo: init with cons. 
 */
SCM
Stem_tremolo::dim_callback (SCM e, SCM)
{
  Grob * se = unsmob_grob (e);
  
  Real space = Staff_symbol_referencer::staff_space (se);
  return ly_interval2scm (Interval (-space, space));
}

/*
  ugh ?  --from Slur
 */
MAKE_SCHEME_CALLBACK (Stem_tremolo, height, 2);
SCM
Stem_tremolo::height (SCM smob, SCM ax)
{
  Axis a = (Axis)ly_scm2int (ax);
  Grob * me = unsmob_grob (smob);
  assert (a == Y_AXIS);

  SCM mol = me->get_uncached_stencil ();

  if (Stencil *m = unsmob_stencil (mol))
    return ly_interval2scm (m->extent (a));
  else
    return ly_interval2scm (Interval ());
}


Stencil
Stem_tremolo::raw_stencil (Grob *me)
{
  Grob *stem = unsmob_grob (me->get_property ("stem"));
  Grob *beam = Stem::get_beam (stem);
  
  Real dydx;
  if (beam)
    {
      Real dy = 0;
      SCM s = beam->get_property ("positions");
      if (is_number_pair (s))
	{
	  dy = -ly_scm2double (ly_car (s)) +ly_scm2double (ly_cdr (s));
	}
      
      Real dx = Beam::last_visible_stem (beam)->relative_coordinate (0, X_AXIS)
	- Beam::first_visible_stem (beam)->relative_coordinate (0, X_AXIS);
      dydx = dx ? dy/dx : 0;
    }
  else
    // urg
    dydx = 0.25;

  Real ss = Staff_symbol_referencer::staff_space (me);
  Real thick = robust_scm2double (me->get_property ("beam-thickness"),1);
  Real width = robust_scm2double (me->get_property ("beam-width"),1);
  Real blot = me->get_paper ()->get_dimension (ly_symbol2scm ("blotdiameter"));

  width *= ss;
  thick *= ss;
  
  Stencil a (Lookup::beam (dydx, width, thick, blot));
  a.translate (Offset (-width/2, width / 2 * dydx));
  
  int tremolo_flags = 0;
  SCM s = me->get_property ("flag-count");
  if (ly_c_number_p (s))
    tremolo_flags = ly_scm2int (s);

  if (!tremolo_flags)
    {
      programming_error ("No tremolo flags?");

      me->suicide ();
      return Stencil ();
    }

  /*
    Who the fuck is 0.81 ?

    --hwn.
   */
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


MAKE_SCHEME_CALLBACK (Stem_tremolo,print,1);
SCM
Stem_tremolo::print (SCM grob) 
{
  Grob *me = unsmob_grob (grob);
  Grob *stem = unsmob_grob (me->get_property ("stem"));
  if (!stem)
    {
      programming_error ("No stem for stem-tremolo");
      return SCM_EOL;
    }
  
  Grob *beam = Stem::get_beam (stem);
  Direction stemdir = Stem::get_direction (stem);
  Real beam_translation
    = (beam && beam->live ())
    ? Beam::get_beam_translation (beam)
    : 0.81;

  Stencil mol = raw_stencil (me);
  Interval mol_ext = mol.extent (Y_AXIS);
  Real ss = Staff_symbol_referencer::staff_space (me);

  // ugh, rather calc from Stem_tremolo_req
  int beam_count = (beam) ? (Stem::beam_multiplicity (stem).length () + 1): 0;


  Real beamthickness = 0.0;
  SCM sbt = (beam) ? beam->get_property ("thickness") : SCM_EOL ;
  if (ly_c_number_p (sbt))
    {
      beamthickness = ly_scm2double (sbt) * ss;
    }

  Real end_y
    = Stem::stem_end_position (stem) *ss/2 
    - stemdir * (beam_count * beamthickness
		 + ((beam_count -1) >? 0) * beam_translation);

  /*
    the 0.33 ss is to compensate for the size of the note head
   */
  Real chord_start_y = Stem::chord_start_y (stem) +
    0.33 * ss * stemdir;

  Real padding = beam_translation;

  /*
    if there is a flag, just above/below the notehead.
    if there is not enough space, center on remaining space,
    else one beamspace away from stem end.
   */
  if (!beam && Stem::duration_log (stem) >= 3)
    {
      mol.align_to (Y_AXIS, -stemdir);
      mol.translate_axis (chord_start_y + .5 * stemdir, Y_AXIS);
    }
  else if (stemdir * (end_y - chord_start_y) - 2*padding - mol_ext.length ()  < 0.0)
    {
      mol.translate_axis (0.5 * (end_y + chord_start_y)  - mol_ext.center (),Y_AXIS);
    }
  else
    {
      mol.translate_axis (end_y - stemdir * beam_translation
			  -mol_ext [stemdir]
			  , Y_AXIS);
    }
  
  return mol.smobbed_copy ();
}



ADD_INTERFACE (Stem_tremolo,"stem-tremolo-interface",
  "A beam slashing a stem to indicate a tremolo.",
  "stem beam-width beam-thickness flag-count");
