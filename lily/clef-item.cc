/*
  clef-item.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <ctype.h>
#include "clef-item.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "clef-engraver.hh"
#include "text-item.hh"

void
Clef_item::do_pre_processing()
{
  bool b= (break_status_dir() != RIGHT);
  change_b_ = b;

  if (default_b_)
    {
      set_empty(b);
      transparent_b_ = b;
    }
}

Clef_item::Clef_item()
{
  breakable_b_ =true;
  default_b_ = false;
  change_b_ = true;
  octave_dir_ = CENTER;
  symbol_ = "violin";
  y_position_i_ = -2;
  
  // Ugh: This should be const, I guess.
  octave_marker_td_p_.set_p (new Text_def());
  octave_marker_td_p_->text_str_ = "8";
  octave_marker_td_p_->style_str_ = "italic";
}


void
Clef_item::read (Clef_engraver const &k)
{
  symbol_ = k.clef_type_str_;
  y_position_i_ = k.clef_position_i_;
  octave_dir_ = k.octave_dir_;
}

Molecule*
Clef_item::brew_molecule_p() const
{
  String t = symbol_;
  if  (change_b_)
    t += "_change";
  Atom s = lookup_l ()->clef (t);
  Molecule*output = new Molecule (Atom (s));
  output->translate_axis (paper()->internote_f () * y_position_i_, Y_AXIS);
  if (octave_dir_) {
    Molecule octave_marker = Molecule(octave_marker_td_p_->get_atom(paper(),
								CENTER));
    Real offset = output->extent()[Y_AXIS][octave_dir_]
		   - octave_marker.extent()[Y_AXIS][- octave_dir_];
    if (octave_dir_ == DOWN)
      offset += octave_marker.extent()[Y_AXIS][UP] * 0.35 ;
    octave_marker.translate_axis (offset, Y_AXIS);
    output->add_molecule (octave_marker);
  }
  return output;
}


IMPLEMENT_IS_TYPE_B1(Clef_item,Item);

#include "pointer.tcc"
template class P<Text_def>;	// ugh


