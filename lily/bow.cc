/*
  bow.cc -- implement Bow

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "bow.hh"
#include "paper-def.hh"
#include "lookup.hh"

Bow::Bow()
{
    left_pos_i_ = right_pos_i_ = 0;
    left_dx_f_ = right_dx_f_ = 0.0;
}


Offset
Bow::center() const
{
    int dy =  right_pos_i_-left_pos_i_;

    Real w = width().length();

    return Offset(w/2,dy * paper()->internote_f());
}


Molecule*
Bow::brew_molecule_p() const
{
    Molecule*output = new Molecule;
    Real w = width().length();
    
    int dy = right_pos_i_ - left_pos_i_;
    
    Real nw_f = paper()->note_width();
    Real nh_f = paper()->internote_f();

    
    w+= (right_dx_f_ - left_dx_f_) * nw_f ;
    Real round_w = w;		// slur lookup rounds the slurwidth .
    
    Symbol sl = paper()->lookup_l()->slur(dy , round_w, dir_i_);

    Real error = w-round_w;
    
    Atom a(sl);
    a.translate(Offset((left_dx_f_ + 0.5 )*nw_f + error/2,
		       left_pos_i_ * nh_f));
    output->add(a);
    return output;
}

IMPLEMENT_STATIC_NAME(Bow);
