/*
  staff-side.cc -- implement Staff_side

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "dimen.hh"
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
    inter_f_ = 2 PT;
    staff_size_i_ = 0;
    staff_sym_l_=0;
    elem_l_ = elem_l;
    dir_i_ =0;
    inside_staff_b_ =false;
}

void
Staff_side::read_staff_sym()
{
    if (! staff_sym_l_)
	return ;
    inter_f_ = staff_sym_l_->inter_note_f();
    staff_size_i_ = staff_sym_l_->steps_i();
}


Interval
Staff_side::support_height() const return r;
{
    
    for (int i=0; i < support_l_arr_.size(); i++)
	r.unite(support_l_arr_[i]->height());
    if (r.empty_b()) {
	r = Interval(0,0);
    }
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
    ((Staff_side*)this)->read_staff_sym();
    if (!dir_i_) {
	warning("Staff_side::get_position_i(): " 
		"somebody forgot to set my vertical direction, returning -20");
	return -20;
    }
    

    Real y=0;
    if (!inside_staff_b_) {
	y  = (dir_i_ > 0) ? staff_size_i_ + 2: -2; 
	y *=inter_f_;
	Interval v= support_height();

	if (dir_i_ > 0) {
	    y = y >? (v.max() + 2*inter_f_);
	} else if (dir_i_ < 0) {
	    y = y <? (v.min() - 2*inter_f_);
	}
    } else {
	Interval v= support_height();
	y = v[dir_i_]  + 2*dir_i_*inter_f_;	// ugh
    }
    return int(rint(Real(y)/inter_f_)); // should ret a float?
}

int
Staff_side::get_position_i(Interval sym_dim) const
{ 
    int i= get_position_i();
    return i+ int(rint(- sym_dim[dir_i_] / inter_f_));
}
