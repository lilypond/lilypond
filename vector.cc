#include "debug.hh"
#include "vector.hh"
#include "string.hh"

Vector::Vector(const Vector&n)
          :dat(n.dat)
     // this makes GCC 272 barf
{
    //dat = n.dat;
}    

Vector::operator String() const
{
    int i=0;
    String s("vector [");
    for (; i < dim(); i++) {
	s += String(dat[i], "%6f") + ' ';
    }
    s+="]";
    return s;
}


void
Vector::print() const
{
    mtor << *this<<'\n';
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
