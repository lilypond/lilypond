/*
  textitem.cc -- implement Text_item

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "musical-request.hh"
#include "paper-def.hh"
#include "text-item.hh"
#include "stem.hh"
#include "molecule.hh"
#include "lookup.hh"

Text_item::Text_item(Text_def *tdef_l)
    : Staff_side(this)
{
    dir_i_ =-1;
    init(tdef_l);  
}

Text_def*
Text_item::tdef_l()
{
    return tdef_p_;
}

Text_item::~Text_item()
{
    delete tdef_p_;
}

void
Text_item::init(Text_def *tdef_l)
{
    tdef_p_ = new Text_def (*tdef_l);
}

Text_item::Text_item(Text_req* treq_l)
    : Staff_side(this)
{
    init(treq_l->tdef_p_);
    dir_i_ = treq_l->dir_i_;
    if (!dir_i_)
	dir_i_ = -1;
}

void
Text_item::set_default_index()
{
    pos_i_  = get_position_i();
}

void
Text_item::do_pre_processing()
{
    set_default_index();
}

    
Molecule*
Text_item::brew_molecule_p() const
{
    Molecule* mol_p = new Molecule(tdef_p_->create_atom(paper()));

    if(dir_i_<0 )		// should do something better anyway.
	mol_p->translate(Offset(0, -mol_p->extent().y.left ));
    mol_p->translate(Offset(0, pos_i_ * paper()->internote()));
    
    return mol_p;
}

IMPLEMENT_STATIC_NAME(Text_item);
