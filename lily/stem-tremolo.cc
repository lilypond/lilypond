/*   
  abbrev.cc --  implement Stem_tremolo
  
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

Stem_tremolo::Stem_tremolo ()
{
  set_elt_property ("stem", SCM_EOL);
}


Stem *
Stem_tremolo::stem_l ()const
{
  SCM s =   get_elt_property ("stem");

  return dynamic_cast<Stem*> (  unsmob_element (s));
}

Interval
Stem_tremolo::dim_callback (Dimension_cache const *c) 
{
  Stem_tremolo * s = dynamic_cast<Stem_tremolo*> (c->element_l ());
  Real space = Staff_symbol_referencer_interface (s->stem_l ())
    .staff_space ();
  return Interval (-space, space);
}


Molecule*
Stem_tremolo::do_brew_molecule_p () const
{
  Stem * st = stem_l ();
  int mult =0;
  if (Beam * b = st->beam_l ())
    {
      mult = b->get_multiplicity ();
    }
  
  Real interbeam_f = paper_l ()->interbeam_f (mult);
  Real w  = gh_scm2double (get_elt_property ("beam-width"));
  Real space = Staff_symbol_referencer_interface (st).staff_space ();
  Real half_staff_space = space / 2;
  Real beam_f = gh_scm2double (get_elt_property ("beam-thickness"));

  int beams_i = 0;
  Real dydx = 0.25;
  
  if (st && st->beam_l ())
    {
      Real dy = 0;
      SCM s = st->beam_l ()->get_elt_property ("height");
      if (gh_number_p (s))
	dy = gh_scm2double (s);
      Real dx = st->beam_l ()->last_visible_stem ()->hpos_f ()
	- st->beam_l ()->first_visible_stem ()->hpos_f ();
      dydx = dy/dx;
  
      // ugh, rather calc from Stem_tremolo_req
      beams_i = st->beam_count(RIGHT) >? st->beam_count (LEFT);
    } 

  Molecule a (lookup_l ()->beam (dydx, w, beam_f));
  a.translate (Offset (-w/2, w / 2 * dydx));
  
  int abbrev_flags = 1;
  {
    SCM a = get_elt_property ("abbrev-flags");
    if (gh_number_p (a))
      abbrev_flags = gh_scm2int (a);
  }

  Molecule *beams= new Molecule; 
  for (int i = 0; i < abbrev_flags; i++)
    {
      Molecule b (a);
      b.translate_axis (interbeam_f * i, Y_AXIS);
      beams->add_molecule (b);
    }
  beams->translate_axis (-beams->extent ()[Y_AXIS].center (), Y_AXIS);

  if (st)
    { 
      if (st->beam_l ())
        {
	  beams->translate (Offset(st->hpos_f () - hpos_f (),
	    st->stem_end_position () * half_staff_space - 
	    directional_element (st->beam_l ()).get () * beams_i * interbeam_f));
	}
      else
	{  
	  /*
	    Beams should intersect one beamthickness below staff end
	   */
	  Real dy = - beams->extent ()[Y_AXIS].length () / 2 * st->get_direction ();

	  /*
	    uhg.  Should use relative coords and placement
	  */
	  Real whole_note_correction = (st && st->invisible_b( ))
	    ? 0.0 //  -st->get_direction () * st->note_delta_f ()/2
	    : 0.0;
	 
	  dy += st->stem_end_position ();
	  beams->translate (Offset(st->hpos_f () - hpos_f ()+
				   whole_note_correction, dy));
	}

      /*
	there used to be half a page of code that was long commented out.
	Removed in 1.1.35
       */
    }
  
  return beams;
}


void
Stem_tremolo::set_stem (Stem *s)
{
  set_elt_property ("stem", s->self_scm_);
  add_dependency (s);
}

