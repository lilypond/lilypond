/*
  staff-side.cc -- implement Staff_side

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "staff-side.hh"
#include "staff-sym.hh"
#include "debug.hh"

void
Staff_side::set_staffsym(Staff_symbol* s_l)
{
    staff_sym_l_ = s_l;
}

Staff_side::Staff_side(Staff_elem * elem_l)
{
    elem_l_ = elem_l;
    dir_i_ =0;
    staff_sym_l_ =0;
    inside_staff_b_ =false;
}


Interval
Staff_side::support_height() const return r;
{
    for (int i=0; i < support_l_arr_.size(); i++)
	r.unite(support_l_arr_[i]->height());
}

void
Staff_side::add_support(Staff_elem*i)
{
    support_l_arr_.push(i);
    elem_l_->add_dependency(i);
}

int
Staff_side::get_position_i()const
{
    if (!staff_sym_l_)
	return 0;
    if (!dir_i_) {
	warning("Staff_side::get_position_i(): returning -20");
	return -20;
    }
    
    Real inter_f = staff_sym_l_->inter_note_f();
    int staff_size_i = staff_sym_l_->steps_i();
    Real y=0;
    if (!inside_staff_b_) {
	y  = (dir_i_ > 0) ? staff_size_i + 2: -2; 
	y *=inter_f;
	Interval v= support_height();

	if (dir_i_ > 0) {
	    y = y >? (v.max() + 2*inter_f);
	} else if (dir_i_ < 0) {
	    y = y <? (v.min() - 2*inter_f);
	}
    } else {
	Interval v= support_height();
	y = v[dir_i_]  + 2*dir_i_*inter_f;	// ugh
    }
    return int(rint(Real(y)/inter_f)); // should ret a float?
}

int
Staff_side::get_position_i(Interval sym_dim) const
{ 
    if (!staff_sym_l_)
	return 0;
   if (!dir_i_) {
	warning("Staff_side::get_position_i(): returning -20");
	return -20;
    }
   
    Real inter_f = staff_sym_l_->inter_note_f();
  
    int i= get_position_i();
    return i+ int(rint(- sym_dim[dir_i_] / inter_f));
}
