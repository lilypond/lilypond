/*
  staff-side.cc -- implement Staff_side

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "interval.hh"
#include "paper-def.hh"
#include "dimen.hh"
#include "staff-side.hh"
#include "staff-sym.hh"
#include "debug.hh"

void
Staff_side::set_staffsym (Staff_symbol* s_l)
{
  staff_sym_l_ = s_l;
  add_dependency (s_l);
}

Staff_side::Staff_side()
{
  pos_i_ =0;
  sym_int_ = Interval (0,0);
  staff_size_i_ = 0;
  staff_sym_l_ = 0;
  dir_ = CENTER;
  inside_staff_b_ = false;
}

void
Staff_side::read_staff_sym()
{
  if (! staff_sym_l_)
    return ;
  staff_size_i_ = staff_sym_l_->steps_i();
}


Interval
Staff_side::support_height() const
{
  Interval r;
  
  for (int i=0; i < support_l_arr_.size(); i++)
    r.unite (support_l_arr_[i]->height());
  if (r.empty_b()) 
    {
      r = Interval (0,0);
    }
  return r;
}

void
Staff_side::add_support (Score_elem*i)
{
  support_l_arr_.push (i);
  add_dependency (i);
}

int
Staff_side::get_position_i() const
{
  if (!dir_) 
    {
      warning ("Staff_side::get_position_i(): " 
	       "somebody forgot to set my vertical direction, returning -20");
      return -20;
    }
  

  Real y=0;
  Real inter_f = paper()-> internote_f ();
  if (!inside_staff_b_) 
    {
      y  = (dir_ > 0 && staff_sym_l_) ? staff_sym_l_->steps_i() + 2: -2; 
      y *=inter_f;
      Interval v= support_height();

      if (dir_ > 0) 
	{
	  y = y >? (v.max() + 2*inter_f);
	}
      else if (dir_ < 0) 
	{
	  y = y <? (v.min() - 2*inter_f);
	}
    }
  else 
    {
      Interval v= support_height();
      y = v[dir_]  + 2*dir_*inter_f;	// ugh
    }
  return int (rint (Real (y)/inter_f)); // should ret a float?
}

Interval
Staff_side::symbol_height() const
{
  return Interval (0,0);
}

void
Staff_side::do_post_processing()
{
  sym_int_ = symbol_height();
  pos_i_ = get_position_i();
  if (dir_)
    pos_i_ += int (rint (- sym_int_[-dir_] / paper()->internote_f ()));
}

void
Staff_side::do_substitute_dependency (Score_elem*o, Score_elem*n)
{ 
  support_l_arr_.unordered_substitute (o,n);
  if (staff_sym_l_ == o)
    staff_sym_l_ = n ? (Staff_symbol*) n->spanner():0;
}


IMPLEMENT_IS_TYPE_B1(Staff_side, Score_elem);
