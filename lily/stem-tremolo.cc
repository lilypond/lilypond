/*   
  abbrev.cc --  implement Stem_tremolo
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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


Stem_tremolo::Stem_tremolo ()
{
  set_elt_property ("stem", SCM_EOL);
  abbrev_flags_i_ = 1;
}

void
Stem_tremolo::do_print () const
{
  DEBUG_OUT << "abbrev_flags_i_ " << abbrev_flags_i_;
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
    .staff_line_leading_f ();
  return Interval (-space, space);
}


Molecule*
Stem_tremolo::do_brew_molecule_p () const
{
  Stem * st = stem_l ();
  int mult =0;
  if (Beam * b = st->beam_l ())
    {
      mult = b->multiplicity_i ();
    }
  
  Real interbeam_f = paper_l ()->interbeam_f (mult);
  Real w  = gh_scm2double (get_elt_property ("beam-width"));
  Real space = Staff_symbol_referencer_interface (st).staff_line_leading_f ();
  Real internote_f = space / 2;
  Real beam_f = gh_scm2double (get_elt_property ("beam-thickness"));

  int beams_i = 0;
  Real slope_f = 0.25;

  if (st && st->beam_l ()) {
    slope_f = st->beam_l ()->slope_f_;
    // ugh, rather calc from Stem_tremolo_req
    beams_i = st->beam_count (RIGHT) >? st->beam_count (LEFT);
  } 

  Molecule a (lookup_l ()->beam (slope_f, w, beam_f));
  a.translate (Offset (-w/2, w / 2 * slope_f));

  Molecule *beams= new Molecule; 
  for (int i = 0; i < abbrev_flags_i_; i++)
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
	    st->stem_end_f () * internote_f - 
	    st->beam_l ()->get_direction () * beams_i * interbeam_f));
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
	    ? -st->get_direction () * st->note_delta_f ()/2
	    : 0.0;

	  dy += st->stem_end_f ();
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

