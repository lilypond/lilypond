/*   
  stem-tremolo.cc --  implement Stem_tremolo
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "stem-tremolo.hh"
#include "debug.hh"
#include "beam.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "stem.hh"
#include "offset.hh"
#include "dimension-cache.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"

/*
  TODO:
    lengthen stem if necessary
 */

Stem_tremolo::Stem_tremolo (SCM s)
  : Item (s)
{
  set_elt_pointer ("stem", SCM_EOL);
}


Stem *
Stem_tremolo::stem_l ()const
{
  SCM s =   get_elt_pointer ("stem");

  return dynamic_cast<Stem*> (  unsmob_element (s));
}

Interval
Stem_tremolo::dim_callback (Score_element * se, Axis )
{
  Stem_tremolo * s = dynamic_cast<Stem_tremolo*> (se);
  Real space = Staff_symbol_referencer_interface (s->stem_l ())
    .staff_space ();
  return Interval (-space, space);
}



GLUE_SCORE_ELEMENT(Stem_tremolo,brew_molecule);
SCM
Stem_tremolo::member_brew_molecule () const
{
  Stem * stem = stem_l ();
  Beam * beam = stem->beam_l ();
  
  Real dydx;
  if (beam)
    {
      Real dy = 0;
      SCM s = beam->get_elt_property ("height");
      if (gh_number_p (s))
	dy = gh_scm2double (s);
      Real dx = beam->last_visible_stem ()->relative_coordinate (0, X_AXIS)
	- beam->first_visible_stem ()->relative_coordinate (0, X_AXIS);
      dydx = dx ? dy/dx : 0;
    }
  else
    // urg
    dydx = 0.25;

  Real ss = Staff_symbol_referencer_interface (stem).staff_space ();
  Real thick = gh_scm2double (get_elt_property ("beam-thickness"));
  Real width = gh_scm2double (get_elt_property ("beam-width"));
  width *= ss;
  thick *= ss;
  
  Molecule a (lookup_l ()->beam (dydx, width, thick));
  a.translate (Offset (-width/2, width / 2 * dydx));
  
  int tremolo_flags;
  SCM s = get_elt_property ("tremolo-flags");
  if (gh_number_p (s))
    tremolo_flags = gh_scm2int (s);
  else
    // huh?
    tremolo_flags = 1;

  int mult = beam ? beam->get_multiplicity () : 0;
  Real interbeam_f = paper_l ()->interbeam_f (mult);
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
      int beams_i = stem->beam_count(RIGHT) >? stem->beam_count (LEFT);
      mol.translate (Offset(stem->relative_coordinate (0, X_AXIS) - relative_coordinate (0, X_AXIS),
			    stem->stem_end_position () * ss / 2 - 
			    directional_element (beam).get () * beams_i * interbeam_f));
    }
  else
    {  
      /*
	Beams should intersect one beamthickness below stem end
      */
      Real dy = stem->stem_end_position () * ss / 2;
      dy -= mol.extent (Y_AXIS).length () / 2 *  stem->get_direction ();

      /*
	uhg.  Should use relative coords and placement
      */
      Real whole_note_correction;
      if (stem->invisible_b ())
	whole_note_correction = -stem->get_direction ()
	  * stem->support_head ()->extent (X_AXIS).length () / 2;
      else
	whole_note_correction = 0;
	 
      mol.translate (Offset (stem->relative_coordinate (0, X_AXIS) - relative_coordinate (0, X_AXIS) +
			     whole_note_correction, dy));
    }
  
  return mol.create_scheme();
}


void
Stem_tremolo::set_stem (Stem *s)
{
  set_elt_pointer ("stem", s->self_scm_);
  add_dependency (s);
}

