/*   
  stem-tremolo.cc --  implement Stem_tremolo
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "stem-tremolo.hh"
#include "debug.hh"
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

void
Stem_tremolo::set_interface (Grob *me)
{
  me->set_interface (ly_symbol2scm ("stem-tremolo"));
}

bool
Stem_tremolo::has_interface (Grob *me)
{
  return me->has_interface (ly_symbol2scm ("stem-tremolo"));
}

MAKE_SCHEME_CALLBACK(Stem_tremolo,dim_callback,2);

/*
  todo: init with cons. 
 */
SCM
Stem_tremolo::dim_callback (SCM e, SCM )
{
  Grob * se = unsmob_grob (e);
  
  Real space = Staff_symbol_referencer::staff_space (se);
  return ly_interval2scm ( Interval (-space, space));
}

/*
  ugh ?  --from Slur
 */
MAKE_SCHEME_CALLBACK (Stem_tremolo, height, 2);
SCM
Stem_tremolo::height (SCM smob, SCM ax)
{
  Axis a = (Axis)gh_scm2int (ax);
  Grob * me = unsmob_grob (smob);
  assert ( a == Y_AXIS);

  SCM mol = me->get_uncached_molecule ();
  return ly_interval2scm (unsmob_molecule (mol)->extent (a));
}


MAKE_SCHEME_CALLBACK(Stem_tremolo,brew_molecule,1);
SCM
Stem_tremolo::brew_molecule (SCM smob)
{
  Grob *me= unsmob_grob (smob);
  Grob * stem = unsmob_grob (me->get_grob_property ("stem"));
  Grob * beam = Stem::beam_l (stem);
  
  Real dydx;
  if (beam)
    {
      Real dy = 0;
      SCM s = beam->get_grob_property ("dy");
      if (gh_number_p (s))
	dy = gh_scm2double (s);
      Real dx = Beam::last_visible_stem (beam)->relative_coordinate (0, X_AXIS)
	- Beam::first_visible_stem (beam)->relative_coordinate (0, X_AXIS);
      dydx = dx ? dy/dx : 0;
    }
  else
    // urg
    dydx = 0.25;

  Real ss = Staff_symbol_referencer::staff_space (stem);
  Real thick = gh_scm2double (me->get_grob_property ("beam-thickness"));
  Real width = gh_scm2double (me->get_grob_property ("beam-width"));
  width *= ss;
  thick *= ss;
  
  Molecule a (Lookup::beam (dydx, width, thick));
  a.translate (Offset (-width/2, width / 2 * dydx));
  
  int tremolo_flags;
  SCM s = me->get_grob_property ("tremolo-flags");
  if (gh_number_p (s))
    tremolo_flags = gh_scm2int (s);
  else
    // huh?
    tremolo_flags = 1;

  int mult = beam ? Beam::get_multiplicity (beam) : 0;
  SCM space_proc = me->get_grob_property ("beam-space-function");
  SCM space = gh_call1 (space_proc, gh_int2scm (mult));
  Real interbeam_f = gh_scm2double (space) * ss;


  Molecule mol; 
  for (int i = 0; i < tremolo_flags; i++)
    {
      Molecule b (a);
      b.translate_axis (interbeam_f * i, Y_AXIS);
      mol.add_molecule (b);
    }
  if (tremolo_flags)
    mol.translate_axis (-mol.extent (Y_AXIS).center (), Y_AXIS);
  if (beam)
    {
      // ugh, rather calc from Stem_tremolo_req
      int beams_i = Stem::beam_count(stem, RIGHT) >? Stem::beam_count (stem, LEFT);
      mol.translate (Offset(stem->relative_coordinate (0, X_AXIS) - me->relative_coordinate (0, X_AXIS),
			    Stem::stem_end_position (stem ) * ss / 2 - 
			    Directional_element_interface::get (beam) * beams_i * interbeam_f));
    }
  else
    {  
      /*
	Beams should intersect one beamthickness below stem end
      */
      Real dy = Stem::stem_end_position (stem ) * ss / 2;
      dy -= mol.extent (Y_AXIS).length () / 2 *  Stem::get_direction (stem );

      /*
	uhg.  Should use relative coords and placement
      */
      Real whole_note_correction;
      if (Stem::invisible_b (stem ))
	{
	  Grob *hed = Stem::support_head (stem );
	  whole_note_correction = -Stem::get_direction (stem )
	    *hed->extent (hed, X_AXIS).length () / 2;
	}
      else
	whole_note_correction = 0;
	 
      mol.translate (Offset (stem->relative_coordinate (0, X_AXIS) - me->relative_coordinate (0, X_AXIS) +
			     whole_note_correction, dy));
    }
  
  return mol.smobbed_copy ();
}


void
Stem_tremolo::set_stem (Grob*me,Grob *s)
{
  me->set_grob_property ("stem", s->self_scm ());
}

