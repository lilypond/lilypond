#include "request.hh"
#include "paperdef.hh"
#include "textitem.hh"
#include "stem.hh"
#include "molecule.hh"
#include "lookup.hh"

NAME_METHOD(Text_item);

Text_item::Text_item(Text_req* treq_l, int staffsize_i)
{
    staffsize_i_ = staffsize_i;
    dir_i_ = treq_l->dir_i_;
    if (!dir_i_)
	dir_i_ = -1;
    tdef_l_ = treq_l->tdef_p_;
}

void
Text_item::set_default_pos()
{
    pos_i_  = (dir_i_ > 0) ? staffsize_i_ + 2: -4;
}

void
Text_item::do_pre_processing()
{
    set_default_pos();
}

    
Molecule*
Text_item::brew_molecule_p() const
{
    Molecule* mol_p = new Molecule(tdef_l_->create_atom(paper()));
    mol_p->translate(Offset(0, pos_i_ * paper()->internote()));

    if(dir_i_<0)
	mol_p->translate(Offset(0, -mol_p->extent().y.length() ));
    
    return mol_p;
}
