#include "vector.hh"
#include "string.hh"

Vector::Vector(const Vector&n)
          :dat(n.dat)
{
}    

Vector
Vector::operator-() const
{
    Vector v(*this); v*=-1; return v;
}

void
Vector::set_unit(int j)
{
    fill(0.0);
    dat[j] = 1.0;
}
