/*
  local-key-item.cc -- implement Local_key_item, Local_acc

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "local-key-item.hh"
#include "molecule.hh"
#include "scalar.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "musical-request.hh"
#include "notehead.hh"
#include "misc.hh"



Local_key_item::Local_key_item(int i)
{
    c0_position  = i;
}

void
Local_key_item::add(Item*head_l)
{
    support_items_.push(head_l);
    add_dependency(head_l);
}

void
Local_key_item::add(Melodic_req*m_l)
{
    add(m_l->octave_i_, m_l->notename_i_, m_l->accidental_i_);
}

void
Local_key_item::add (int o, int p , int a)
{
    Local_acc l;
    l.octave_i_ = o;
    l.name_i_ = p;
    l.accidental_i_ = a;
    for (int i=0; i< accs.size(); i++)
	if (!Local_acc::compare(l, accs[i]))
	    return;
    
    accs.push(l);
}

void
Local_key_item::do_pre_processing()
{
    accs.sort(Local_acc::compare);
}

Molecule*
Local_key_item::brew_molecule_p()const
{
    Molecule* output = new Molecule;
    Molecule*octmol = 0;
    int lastoct = -100;
    for  (int i = 0; i <  accs.size(); i++) {
	// do one octave
	if (accs[i].octave_i_ != lastoct) {
	    if (octmol){
		Real dy =lastoct*7*paper()->internote();
		octmol->translate(Offset(0, dy));
		output->add(*octmol);
		delete octmol;
	    }
	    octmol= new Molecule;
	}
	lastoct = accs[i].octave_i_;
	Symbol s =paper()->lookup_l()->accidental(accs[i].accidental_i_);   
	Atom a(s);
	Real dy = (accs[i].name_i_ + c0_position) * paper()->internote();
	a.translate(Offset(0,dy));

	octmol->add_right(a);
    }

    if (octmol){
	Real dy =lastoct*7*paper()->internote();
	octmol->translate(Offset(0, dy));
	output->add(*octmol);
	delete octmol;
    }

    Interval head_width=itemlist_width(support_items_);
    output->translate(Offset(-output->extent().x.right + head_width.left ,0));
    
    return output;
}

int
Local_acc::compare(Local_acc&a, Local_acc&b)
{
    if (a.octave_i_ - b.octave_i_)
	return a.octave_i_ - b.octave_i_;
    if (a.name_i_ - b.name_i_)
	return a.name_i_ - b.name_i_;
    
    return a.accidental_i_ - b.accidental_i_;
};
IMPLEMENT_STATIC_NAME(Local_key_item);
