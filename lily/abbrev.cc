/*   
  abbrev.cc --  implement Abbreviation
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "abbrev.hh"
#include "debug.hh"
#include "beam.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "stem.hh"
#include "offset.hh"

Abbreviation::Abbreviation ()
{
  stem_l_ = 0;
  abbrev_flags_i_ = 1;
}

void
Abbreviation::do_print () const
{
  DOUT << "abbrev_flags_i_ " << abbrev_flags_i_;
}

Molecule*
Abbreviation::do_brew_molecule_p () const
{
  Beam * b = stem_l_->beam_l_;
  int mult =0;
  if (b)
    {
      Stem_info i = b->get_stem_info (stem_l_);
      mult = i.mult_i_;
    }
  
  Real interbeam_f = paper_l ()->interbeam_f (mult);
  Real w = 1.5 * lookup_l ()->notehead (2, "").dim_[X_AXIS].length ();
  Real space = stem_l_->staff_line_leading_f ();
  Real internote_f = space/2;
  
  Real beam_f = paper_l ()->beam_thickness_f ();

  int beams_i = 0;
  Real slope_f = internote_f / 4 / internote_f;	// HUH?

  if (stem_l_ && stem_l_->beam_l_) {
    slope_f = stem_l_->beam_l_->slope_f_;
    // ugh, rather calc from Abbreviation_req
    beams_i = stem_l_->beams_i_drul_[RIGHT] >? stem_l_->beams_i_drul_[LEFT];
  } 
  Real sl = slope_f * internote_f;

  Molecule a (lookup_l ()->beam (sl, w, beam_f));
  a.translate (Offset (-w/2, w / 2 * slope_f));

  Molecule *beams= new Molecule; 
  for (int i = 0; i < abbrev_flags_i_; i++)
    {
      Molecule b (a);
      b.translate_axis (interbeam_f * i, Y_AXIS);
      beams->add_molecule (b);
    }
  beams->translate_axis (-beams->extent ()[Y_AXIS].center (), Y_AXIS);

  if (stem_l_)
    { 
      if (stem_l_->beam_l_)
        {
	  beams->translate (Offset(stem_l_->hpos_f () - hpos_f (),
	    stem_l_->stem_end_f () * internote_f - 
	    stem_l_->beam_l_->dir_ * beams_i * interbeam_f));
	}
      else
	{  
	  /*
	    Beams should intersect one beamthickness below staff end
	   */
	  Real dy = - beams->extent ()[Y_AXIS].length () / 2 * stem_l_->dir_;
	  dy /= internote_f;
	  dy += stem_l_->stem_end_f ();
	  dy *= internote_f;
// urg: can't: stem should be stetched first
//	  dy -= paper_l ()->beam_thickness_f () * stem_l_->dir_;
	  beams->translate (Offset(stem_l_->hpos_f () - hpos_f (), dy));
	}

      /*
	there used to be half a page of code that was long commented out.
	Removed in 1.1.35
       */
    }
  
  return beams;
}

void
Abbreviation::do_substitute_element_pointer (Score_element*o, Score_element*n)
{
  if (stem_l_ == o)
    stem_l_ = dynamic_cast<Stem*> (n);
}


void
Abbreviation::set_stem (Stem *s)
{
  stem_l_ = s;
  add_dependency (s);
}
