/*
  local-key-item.cc -- implement Local_key_item, Local_acc

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "dimen.hh"
#include "local-key-item.hh"
#include "molecule.hh"
#include "scalar.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "musical-request.hh"
#include "note-head.hh"
#include "misc.hh"



Local_key_item::Local_key_item (int i)
{
  c0_position  = i;
}

void
Local_key_item::add_support (Item*head_l)
{
  support_items_.push (head_l);
  add_dependency (head_l);
}

void
Local_key_item::add (Melodic_req*m_l)
{
  add (m_l->octave_i_, m_l->notename_i_, m_l->accidental_i_);
}

void
Local_key_item::add (int o, int p , int a)
{
  Local_acc l;
  l.octave_i_ = o;
  l.name_i_ = p;
  l.accidental_i_ = a;
  for (int i=0; i< accs.size(); i++)
    if (!Local_acc::compare (l, accs[i]))
      return;
  
  accs.push (l);
}

void
Local_key_item::do_pre_processing()
{
  accs.sort (Local_acc::compare);
}

Molecule*
Local_key_item::brew_molecule_p() const
{
  Molecule*output = new Molecule;

  Molecule *octave_mol_p = 0;
  int lastoct = -100;
  for  (int i = 0; i <  accs.size(); i++) 
    {
      // do one octave
      if (accs[i].octave_i_ != lastoct) 
	{
	  if (octave_mol_p)
	    {
	      Real dy =lastoct*7*paper()->internote_f ();
	      octave_mol_p->translate (dy, Y_AXIS);
	      output->add (*octave_mol_p);
	      delete octave_mol_p;
	    }
	  octave_mol_p= new Molecule;
	}
      lastoct = accs[i].octave_i_;
      
      Real dy = (accs[i].name_i_ + c0_position) * paper()->internote_f ();
      Atom a (paper()->lookup_l ()->accidental (accs[i].accidental_i_));
      a.dim_[X_AXIS] += 1 PT; // todo
      a.translate (dy, Y_AXIS);
      Molecule m(a);
      octave_mol_p->add_at_edge (X_AXIS, RIGHT, m);
    }

  if (octave_mol_p)
    {
      Real dy =lastoct*7*paper()->internote_f ();
      octave_mol_p->translate (dy, Y_AXIS);
      output->add (*octave_mol_p);
      delete octave_mol_p;
    }
  
 if (accs.size()) 
    {
      Box b(Interval (0, paper()->internote_f ()), Interval (0,0));
      Molecule m (paper()->lookup_l ()->fill (b));
      output->add_at_edge (X_AXIS, RIGHT, m);
    }
  Interval head_width=itemlist_width (support_items_);
  output->translate (-output->extent().x ().right + head_width.left , X_AXIS);
  
  return output;
}

int
Local_acc::compare (Local_acc&a, Local_acc&b)
{
  if (a.octave_i_ - b.octave_i_)
    return a.octave_i_ - b.octave_i_;
  if (a.name_i_ - b.name_i_)
    return a.name_i_ - b.name_i_;
  
  return a.accidental_i_ - b.accidental_i_;
};

IMPLEMENT_IS_TYPE_B1(Local_key_item,Item);

void
Local_key_item::do_substitute_dependency (Score_elem*o,Score_elem*n)
{
  Item* o_l = o->item();
  Item* n_l = n?n->item():0;

  support_items_.substitute (o_l, n_l);
}
