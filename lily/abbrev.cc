/*   
  abbrev.cc --  implement Abbreviation
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "abbrev.hh"
#include "debug.hh"
#include "beam.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "stem.hh"

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
Abbreviation::brew_molecule_p () const
{
  Real interbeam_f = paper ()->interbeam_f (stem_l_->mult_i_);
  Real w = 1.5 * lookup_l ()->ball (2).dim_.x ().length ();
  Real internote_f = paper ()->internote_f ();
  Real beam_f = paper ()->beam_thickness_f ();

  int beams_i = 0;
  Real slope_f = internote_f / 4 / internote_f;

  if (stem_l_ && stem_l_->beam_l_) {
    slope_f = stem_l_->beam_l_->slope_f_;
    // ugh, rather calc from Abbreviation_req
    beams_i = stem_l_->beams_right_i_ >? stem_l_->beams_left_i_;
  } 
  Real sl = slope_f * internote_f;

  Atom a (lookup_l ()->beam (sl, w, beam_f));
  a.translate (Offset (-w/2, w / 2 * slope_f));

  Molecule *beams= new Molecule; 
  for (int i = 0; i < abbrev_flags_i_; i++)
    {
      Atom b (a);
      b.translate_axis (interbeam_f * i, Y_AXIS);
      beams->add_atom (b);
    }
#define EGCS_ICE
#ifndef EGCS_ICE
  beams->translate_axis (-beams->extent ()[Y_AXIS].center (), Y_AXIS);
#else
  beams->translate_axis (-(beams->extent ()[Y_AXIS].min () + 
    beams->extent ()[Y_AXIS].max ()) / 2 , Y_AXIS);
#endif

  if (stem_l_)
    { 
      if (stem_l_->beam_l_)
        {
	  beams->translate (Offset(stem_l_->hpos_f () - hpos_f (),
	    stem_l_->stem_end_f () * internote_f - 
	    stem_l_->beam_l_->dir_ * beams_i * interbeam_f));
	}
      else
#if 1
	{  
	  /*
	    Beams should intersect one beamthickness below staff end
	   */
	  Real dy = - beams->extent ()[Y_AXIS].length () / 2 * stem_l_->dir_;
	  dy /= internote_f;
	  dy += stem_l_->stem_end_f ();
	  dy *= internote_f;
// urg: can't: stem should be stetched first
//	  dy -= paper ()->beam_thickness_f () * stem_l_->dir_;
	  beams->translate (Offset(stem_l_->hpos_f () - hpos_f (), dy));
	}
#else
	{
	  /* 
	     urg: this is wrong, even if coded correctly

	     Try to be in the middle of the open part of the stem and
	     between on the staff.

	     (urgh)
	  */
	  Direction sd  = stem_l_->dir_;
	  // watch out: chord_start_f is (the only one) not in dim(internote)
	  Interval empty_stem (stem_l_->chord_start_f () / internote_f * sd
	    + interline_f, (stem_l_->stem_end_f ()* sd));
	  empty_stem *= sd;
	  
	  Interval instaff = empty_stem;
	  /*
	    huh? i don't understand, hw
	    what about:
	    .fly= \stemup d'''2:16
	    instaff.intersect (Interval (-4,4));
	    */
	  // hmm, let's try
	  if (stem_l_->get_default_dir () == stem_l_->dir_)
	    instaff.intersect (Interval (-4,4));

	  if (instaff.empty_b ())
	    instaff = empty_stem;

	  instaff.print (); 
	  instaff *= internote_f;
	  beams->translate (Offset(stem_l_->hpos_f () - hpos_f (),
			      instaff.center ()));
	}
#endif
    }
  
  return beams;
}

void
Abbreviation::do_substitute_dependent (Score_element*o, Score_element*n)
{
  if (stem_l_ == o)
    stem_l_ = n ? (Stem*)n->access_Item () : 0;
}


void
Abbreviation::set_stem (Stem *s)
{
  stem_l_ = s;
  add_dependency (s);
}
