/*
  offset.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef OFFSET_HH
#define OFFSET_HH

#include "real.hh"
#include "axes.hh"
#include "arithmetic-operator.hh"

struct Offset;

Offset complex_multiply (Offset, Offset);
Offset complex_divide (Offset, Offset);
Offset complex_exp (Offset);


/** 2d vector
    should change to Complex
*/
struct Offset {
public:
  Real coordinate_a_[NO_AXES];
    
  Real &operator[](Axis i) {
    return coordinate_a_[i];
  }
  Real operator[](Axis i) const{
    return coordinate_a_[i];
  }
    
  Offset& operator+=(Offset o) {
    (*this)[X_AXIS] += o[X_AXIS];
    (*this)[Y_AXIS] += o[Y_AXIS];
    return *this;
  }
  Offset operator - () const {
    Offset o = *this;
    
    o[X_AXIS]  = - o[X_AXIS];
    o[Y_AXIS]  = - o[Y_AXIS];
    return o;
  }
  Offset& operator-=(Offset o) {
    (*this)[X_AXIS] -= o[X_AXIS];
    (*this)[Y_AXIS] -= o[Y_AXIS];

    return *this;
  }
  
  Offset &scale (Offset o) {
    (*this)[X_AXIS] *= o[X_AXIS];
    (*this)[Y_AXIS] *= o[Y_AXIS];

    return *this;
  }
  Offset &operator *=(Real a) {
    (*this)[X_AXIS] *= a;
    (*this)[Y_AXIS] *= a;

    return *this;
  }
      
  Offset (Real ix , Real iy) {
    coordinate_a_[X_AXIS] =ix;
    coordinate_a_[Y_AXIS] =iy;    
  }
  Offset() {
    coordinate_a_[X_AXIS]=
      coordinate_a_[Y_AXIS]=
      0.0;
  }

  String str () const;

  void mirror (Axis);
  Real  arg () const;
  Real length () const;
  Offset operator *=(Offset z2) {
    *this = complex_multiply (*this,z2);
    return *this;
  }

};

IMPLEMENT_ARITHMETIC_OPERATOR(Offset, +);
IMPLEMENT_ARITHMETIC_OPERATOR(Offset, -);
IMPLEMENT_ARITHMETIC_OPERATOR(Offset, *);

inline Offset
operator* (Real o1, Offset o2)
{
  o2 *= o1;
  return o2;
}

inline Offset
operator* (Offset o1, Real o2)
{
  o1 *= o2;
  return o1;
}


#endif // OFFSET_HH


