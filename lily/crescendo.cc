/*
  crescendo.cc -- implement Crescendo

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "dimen.hh"
#include "crescendo.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "debug.hh"

Crescendo::Crescendo()
    : Staff_side(this)
{
    grow_dir_i_ =0;
    dir_i_ = -1 ;
    left_dyn_b_ = right_dyn_b_ =false;
}

Spanner*
Crescendo::do_break_at(PCol*, PCol*)const
{
    return new Crescendo(*this);
}


Molecule*
Crescendo::brew_molecule_p() const return m_p ;
{
    Real x_off_dim=0.0;
    Real absdyn_dim = 10 PT;	// UGR
    
    m_p = new Molecule;
    Real w_dim = width().length();
    if ( left_dyn_b_ ) {
	w_dim -= absdyn_dim;
	x_off_dim += absdyn_dim;
    }
    if ( right_dyn_b_ ) {
	w_dim -= absdyn_dim;
    }
    
    if (w_dim < 0) {
	error("Crescendo too small");
	w_dim = 0;
    }
    Symbol s( paper()->lookup_l()->hairpin(w_dim, grow_dir_i_ < 0) );
    m_p->add(Atom(s));
    int pos = get_position_i(s.dim.y);
    m_p->translate(Offset(x_off_dim,pos * paper()->internote()));
}

IMPLEMENT_STATIC_NAME(Crescendo);
