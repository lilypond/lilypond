/*
  offset.hh -- part of GNU LilyPond

  (c) 1996--2008 Han-Wen Nienhuys
*/

#ifndef OFFSET_HH
#define OFFSET_HH

#include "axis.hh"
#include "std-string.hh"
#include "real.hh"


/*
  This is a mixture a 2D vector. Sometimes it can
  also be convenient to think of 2D vectors as complex numbers
  (ie. x + i y). The naming of some methods reflects that.
*/
class Offset
{
public:
  Real coordinate_a_[NO_AXES];

  Real &operator [] (Axis i)
  {
    return coordinate_a_[i];
  }

  Real operator [] (Axis i) const
  {
    return coordinate_a_[i];
  }

  Offset &operator += (Offset o)
  {
    (*this)[X_AXIS] += o[X_AXIS];
    (*this)[Y_AXIS] += o[Y_AXIS];
    return *this;
  }

  Offset operator - () const
  {
    Offset o = *this;

    o[X_AXIS] = -o[X_AXIS];
    o[Y_AXIS] = -o[Y_AXIS];
    return o;
  }

  Offset &operator -= (Offset o)
  {
    (*this)[X_AXIS] -= o[X_AXIS];
    (*this)[Y_AXIS] -= o[Y_AXIS];

    return *this;
  }

  Offset &scale (Offset o)
  {
    (*this)[X_AXIS] *= o[X_AXIS];
    (*this)[Y_AXIS] *= o[Y_AXIS];

    return *this;
  }

  Offset &operator /= (Real a)
  {
    (*this) *= 1/a;
    return *this;
  }

  Offset &operator *= (Real a)
  {
    (*this)[X_AXIS] *= a;
    (*this)[Y_AXIS] *= a;

    return *this;
  }

  Offset (Real ix, Real iy)
  {
    coordinate_a_[X_AXIS] = ix;
    coordinate_a_[Y_AXIS] = iy;
  }

  Offset ()
  {
    coordinate_a_[X_AXIS] = coordinate_a_[Y_AXIS] = 0.0;
  }

  string to_string () const;

  Offset &mirror (Axis a)
  {
    coordinate_a_[a] = -coordinate_a_[a];
    return *this;
  }
  Offset direction () const;
  Offset swapped () const;
  
  Real arg () const;
  Real angle_degrees () const;
  Real length () const;
  bool is_sane () const;
  Offset operator *= (Offset z2);
};

#include "arithmetic-operator.hh"
IMPLEMENT_ARITHMETIC_OPERATOR (Offset, +);
IMPLEMENT_ARITHMETIC_OPERATOR (Offset, -);
IMPLEMENT_ARITHMETIC_OPERATOR (Offset, *);



Offset complex_multiply (Offset, Offset);
Offset complex_divide (Offset, Offset);
Offset complex_exp (Offset);

inline Offset
Offset::operator *= (Offset z2)
{
  *this = complex_multiply (*this, z2);
  return *this;
}

inline Offset
operator * (Real o1, Offset o2)
{
  o2 *= o1;
  return o2;
}

inline Offset
operator / (Offset o1, Real a)
{
  o1 /= a;
  return o1;
}

inline Offset
operator * (Offset o1, Real o2)
{
  o1 *= o2;
  return o1;
}

inline Offset
mirror (Offset o, Axis a)
{
  o.mirror (a);
  return o;
}

inline
Real
dot_product (Offset o1, Offset o2)
{
  return o1[X_AXIS] * o2[X_AXIS] + o1[Y_AXIS] * o2[Y_AXIS];
}

#endif /* OFFSET_HH */

