/*
  crescendo.cc -- implement Crescendo

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "molecule.hh"
#include "dimen.hh"
#include "crescendo.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "debug.hh"

Crescendo::Crescendo()
{
    grow_dir_i_ =0;
    dir_i_ = -1 ;
    left_dyn_b_ = right_dyn_b_ =false;
    inside_staff_b_ = false;
}

Interval
Crescendo::symbol_height()const
{
    return get_symbol().dim.y;
}

static Real absdyn_dim = 10 PT;	// ugh

Symbol
Crescendo::get_symbol()const
{    
    Real w_dim = width().length();
    if ( left_dyn_b_ ) {
	w_dim -= absdyn_dim;
    }
    if ( right_dyn_b_ ) {
	w_dim -= absdyn_dim;
    }
    
    if (w_dim < 0) {
	warning("Crescendo too small");
	w_dim = 0;
    }
//    Real lookup_wid = w_dim * 0.9; // make it slightly smaller.

    return Symbol( paper()->lookup_l()->hairpin( w_dim, grow_dir_i_ < 0) );
}

Molecule*
Crescendo::brew_molecule_p() const
{
    Molecule* m_p =0;
    Real x_off_dim=0.0;
    if ( left_dyn_b_)
	x_off_dim += absdyn_dim;
    
    m_p = new Molecule;
    Symbol s(get_symbol());
    m_p->add(Atom(s));
    m_p->translate(Offset(x_off_dim, pos_i_ * paper()->internote_f()));
    return m_p;
}


IMPLEMENT_IS_TYPE_B1(Crescendo,Spanner);
