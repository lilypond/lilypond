/*
  local-key-item.cc -- implement Local_key_item, Musical_pitch

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
Local_key_item::add (Musical_pitch p)
{
  for (int i=0; i< accidental_pitch_arr_.size(); i++)
    if (!Musical_pitch::compare (p, accidental_pitch_arr_[i]))
      return;
  
  accidental_pitch_arr_.push (p);
}

void
Local_key_item::do_pre_processing()
{
  accidental_pitch_arr_.sort (Musical_pitch::compare);
}

Molecule*
Local_key_item::brew_molecule_p() const
{
  Molecule*output = new Molecule;

  Molecule *octave_mol_p = 0;
  int lastoct = -100;
  for  (int i = 0; i <  accidental_pitch_arr_.size(); i++) 
    {
      // do one octave
      if (accidental_pitch_arr_[i].octave_i_ != lastoct) 
	{
	  if (octave_mol_p)
	    {
	      Real dy =lastoct*7*paper()->internote_f ();
	      octave_mol_p->translate_axis (dy, Y_AXIS);
	      output->add_molecule (*octave_mol_p);
	      delete octave_mol_p;
	    }
	  octave_mol_p= new Molecule;
	}
      
      lastoct = accidental_pitch_arr_[i].octave_i_;
      Real dy =
	(c0_position_i_ + accidental_pitch_arr_[i].notename_i_)
	* paper()->internote_f ();
      Atom a (lookup_l ()->accidental (accidental_pitch_arr_[i].accidental_i_));

      a.translate_axis (dy, Y_AXIS);
      Molecule m(a);
      octave_mol_p->add_at_edge (X_AXIS, RIGHT, m);
    }

  if (octave_mol_p)
    {
      Real dy =lastoct*7*paper()->internote_f ();
      octave_mol_p->translate_axis (dy, Y_AXIS);
      output->add_molecule (*octave_mol_p);
      delete octave_mol_p;
    }
  
 if (accidental_pitch_arr_.size()) 
    {
      Box b(Interval (0, paper()->internote_f ()), Interval (0,0));
      Molecule m (lookup_l ()->fill (b));
      output->add_at_edge (X_AXIS, RIGHT, m);
    }

  Interval x_int;
  for (int i=0; i < support_items_.size(); i++) 
    {
      Graphical_axis_group *common = 
	common_group (support_items_[i], X_AXIS);

      Real x = support_items_[i]->relative_coordinate (common, X_AXIS)  
	-relative_coordinate (common, X_AXIS);

      x_int.unite (x + support_items_[i]->width());
    }
  if (x_int.empty_b ())
    x_int = Interval(0,0);
  
  output->translate_axis (-output->extent()[X_AXIS][RIGHT] + x_int[LEFT], X_AXIS);
  
  return output;
}

IMPLEMENT_IS_TYPE_B1(Local_key_item,Item);

void
Local_key_item::do_substitute_dependency (Score_element*o,Score_element*n)
{
  Item* o_l = o->access_Item ();
  Item* n_l = n?n->access_Item ():0;

  support_items_.substitute (o_l, n_l);
}
