/*
  local-key-item.cc -- implement Local_key_item, Musical_pitch

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "local-key-item.hh"
#include "molecule.hh"
#include "scalar.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "musical-request.hh"
#include "note-head.hh"
#include "misc.hh"

Local_key_item::Local_key_item ()
{
  c0_position_i_ = 0;
}

void
Local_key_item::add_support (Item*head_l)
{
  if (support_items_.find_l(head_l))
    return ;
  support_items_.push (head_l);
  add_dependency (head_l);
}

void
Local_key_item::add_pitch (Musical_pitch p, bool cautionary)
{
  for (int i=0; i< accidental_arr_.size(); i++)
    if (!Musical_pitch::compare (p, accidental_arr_[i].pitch_))
      return;

  Local_key_cautionary_tuple t;
  t.pitch_ = p;
  t.cautionary_b_ = cautionary;
  accidental_arr_.push (t);
}

void
Local_key_item::do_pre_processing()
{
  accidental_arr_.sort (Local_key_cautionary_tuple::compare);
}

Molecule*
Local_key_item::do_brew_molecule_p() const
{
  Molecule*output = new Molecule;
  Real note_distance = staff_line_leading_f ()/2;
  Molecule *octave_mol_p = 0;
  int lastoct = -100;
  
  for  (int i = 0; i <  accidental_arr_.size(); i++) 
    {
      Musical_pitch p (accidental_arr_[i].pitch_);
      // do one octave
      if (p.octave_i_ != lastoct) 
	{
	  if (octave_mol_p)
	    {
	      Real dy =lastoct*7* note_distance;
	      octave_mol_p->translate_axis (dy, Y_AXIS);
	      output->add_molecule (*octave_mol_p);
	      delete octave_mol_p;
	    }
	  octave_mol_p= new Molecule;
	}
      
      lastoct = p.octave_i_;
      Real dy =
	(c0_position_i_ + p.notename_i_)
	* note_distance;
      Molecule m (lookup_l ()->accidental (p.accidental_i_, 
					   accidental_arr_[i].cautionary_b_));

      m.translate_axis (dy, Y_AXIS);
      octave_mol_p->add_at_edge (X_AXIS, RIGHT, m, 0);
    }

  if (octave_mol_p)
    {
      Real dy =lastoct*7*note_distance;
      octave_mol_p->translate_axis (dy, Y_AXIS);
      output->add_molecule (*octave_mol_p);
      delete octave_mol_p;
    }
  
 if (accidental_arr_.size()) 
    {
      Box b(Interval (0, note_distance), Interval (0,0));
      Molecule m (lookup_l ()->fill (b));
      output->add_at_edge (X_AXIS, RIGHT, m, 0);
    }

  Interval x_int;
  for (int i=0; i < support_items_.size(); i++) 
    {
      Dimension_cache *common = 
	common_group (support_items_[i], X_AXIS);

      Real x = support_items_[i]->relative_coordinate (common, X_AXIS)
	- relative_coordinate (common, X_AXIS);

      x_int.unite (x + support_items_[i]->extent (X_AXIS));
    }
  if (x_int.empty_b ())
    x_int = Interval(0,0);
  
  output->translate_axis (-output->extent()[X_AXIS][RIGHT] + x_int[LEFT], X_AXIS);
  
  return output;
}



void
Local_key_item::do_substitute_element_pointer (Score_element*o,Score_element*n)
{
  if (Item* o_l = dynamic_cast <Item *> (o))
    support_items_.substitute (o_l,dynamic_cast <Item *> (n));
}
