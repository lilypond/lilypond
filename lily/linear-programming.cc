/*   
  linear-programming.cc --  implement 
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
 */

#if 0
#include "linear-programming.hh"

Linear_programming::Linear_programming (int n)
  : cost_vec_ (n)
{
}
int
Linear_programming::dim () const
{
  return cost_vec_.dim ();
}

void
Linear_programming::add_constraint (Vector c, double r)
{
  assert (c.dim () == cost_vec_);
  constraints_.push (c);
  constraint_rhss_.push (r);
}

void
Linear_programming::set_cost (Vector c)
{
  cost_vec_ = c;
}

void
Linear_programming::print () const
{
  DOUT << "cost: " << cost_vec_;
  for (int i=0; constraints_.size (); i++)
    {
      DOUT << constraints_[i] << ". x = " << constraint_rhss_[i];
    }
}

void
Linear_programming::OK () const
{
  assert (constraint_rhss_.size () == constraints_.size ());
  for (int i=0; constraints_.size (); i++)
    constraints_[i].dim () == cost_vec_.dim ();
}


bool
Linear_programming::check_constraints (Vector v) const
{
  bool is_cool = true;
  for (int i=0; is_cool && i < v.dim (); i++)
    is_cool = is_cool && v[i] >= 0;
  for (int i=0; is_cool && i < constraints_.size (); i++)  
    is_cool = is_cool && (constraints_[i] * v <= constraint_rhss_[i]);



  return is_cool;
}

Vector
Linear_programming::solve (Vector initial) const
{
  Array<int> binding, nonbinding;
  
  assert (check_constraints (initial));
  OK ();

  Vector x (initial);
  Real value (x * cost_vec_):
  
  for (int i=0; i < constraints_.size ())
    nonbinding.push (i);

  while  ()
    {
      get_negative_index (
    }
  
}
#endif
