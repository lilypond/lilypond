/*
  script.cc -- implement Script

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "script-def.hh"
#include "musical-request.hh"
#include "paper-def.hh"
#include "script.hh"
#include "stem.hh"
#include "molecule.hh"
#include "lookup.hh"

void
Script::do_print() const
{
#ifndef NPRINT
    specs_l_->print();
#endif
}

void
Script::do_substitute_dependency(Score_elem*o,Score_elem*n)
{
    Staff_side::do_substitute_dependency(o,n);
    if (o == stem_l_) {
	stem_l_ = n ? (Stem*)n->item() : 0;
    }
}

void
Script::set_stem(Stem*st_l)
{
    stem_l_ = st_l;
    add_support(st_l);
}


Script::Script()
{    
    specs_l_ = 0;
    inside_staff_b_ = false;
    stem_l_ = 0;
    dir_i_ =  0;
}

void
Script::set_default_dir()
{
    int s_i=specs_l_->rel_stem_dir_i();
    if (s_i) { 
	if(stem_l_)
	    dir_i_ = stem_l_->dir_i_ * s_i;
	else{ 
	    specs_l_->warning("Script needs stem direction");
	    dir_i_ = -1;
	}
    } else {
	dir_i_ =specs_l_->staff_dir_i();
    }
    assert(dir_i_);
}


Interval
Script::do_width() const
{
    return specs_l_->get_atom(paper(), dir_i_).extent().x;
}

void
Script::do_pre_processing()
{
    if (!dir_i_)
	set_default_dir();
    inside_staff_b_ = specs_l_->inside_b();
}

Interval
Script::symbol_height()const
{
    return specs_l_->get_atom(paper(), dir_i_).extent().y;
}

Molecule*
Script::brew_molecule_p() const
{
    Real dy = paper()->internote_f();
    
    Molecule*out = new Molecule(specs_l_->get_atom(paper(), dir_i_));
    out->translate_y(dy * pos_i_);
    return out;
}

IMPLEMENT_STATIC_NAME(Script);
IMPLEMENT_IS_TYPE_B2(Script,Item,Staff_side);

int 
Script::compare(Script  *const&l1, Script *const&l2) 
{
    return l1->specs_l_->priority_i() - l2->specs_l_->priority_i();
}
    
