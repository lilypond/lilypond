/*   
  vector.cc --  implement Vector
  
  source file of the Flower Library
  
  (c) 1996-98 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "vector.hh"

Vector::Vector (Array<Real> d)
     : dat (d)
{
 
}
Vector::Vector (Vector const &n)
  : dat (n.dat)
{
}    

Vector
Vector::operator-() const
{
  Vector v (*this);
  v*=-1;
  return v;
}

void
Vector::set_unit (int j)
{
  fill (0.0);
  dat[j] = 1.0;
}
