/*   
  abbrev.cc --  implement Abbreviation
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
  
 */

#include "abbrev.hh"
#include "debug.hh"
#include "beam.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "stem.hh"
#include "dimen.hh"

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
  Real dy = paper ()->interbeam_f (stem_l_->mult_i_);
  Real w = 1.5 * paper ()->lookup_l ()->ball (2).dim_.x ().length ();
  Real interline_f = paper ()->interline_f ();

  int beams_i = 0;
  Real slope_f = paper ()->internote_f () / 4;

  if (stem_l_ && stem_l_->beam_l_) {
    // huh?
    slope_f = 2 * stem_l_->beam_l_->slope_f_;
    // ugh, rather calc from Abbreviation_req
    beams_i = stem_l_->beams_right_i_ >? stem_l_->beams_left_i_;
  }
  paper ()->lookup_l ()->beam (slope_f, 20 PT, 1 PT);

  Atom a (paper ()->lookup_l ()->beam (slope_f, w, .48 * interline_f));
  a.translate (Offset (-w/2, w / 2 * slope_f));

  Molecule *beams= new Molecule; 
  for (int i = 0; i < abbrev_flags_i_; i++)
    {
      Atom b (a);
      b.translate_axis (dy * i, Y_AXIS);
      beams->add (b);
    }
  beams->translate_axis (-beams->extent ()[Y_AXIS].center (), Y_AXIS);

  if (stem_l_)
    {
      /* Try to be in the middle of the open part of the stem and
	 between on the staff.

	 (urgh)
      */
      Direction sd  = stem_l_->dir_;
      Interval empty_stem (stem_l_->chord_start_f () * sd ,
			   (stem_l_->stem_end_f ()* sd) - beams_i * dy);
      empty_stem *= sd;
      
      Interval instaff = empty_stem;
      instaff.intersect (interline_f * Interval (-2,2));
      if (instaff.empty_b ())
	instaff = empty_stem;

      instaff.print (); 
      beams->translate (Offset(stem_l_->hpos_f () - hpos_f (),
			  instaff.center ()));
    }
  
  return beams;
}

void
Abbreviation::do_substitute_dependent (Score_elem*o, Score_elem*n)
{
  if (stem_l_ == o)
    stem_l_ = n ? (Stem*)n->item () : 0;
}


void
Abbreviation::set_stem (Stem *s)
{
  stem_l_ = s;
  add_dependency (s);
}
