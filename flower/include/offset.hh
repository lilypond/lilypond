/*
  offset.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef OFFSET_HH
#define OFFSET_HH

#include "real.hh"
#include "axes.hh"

/** 2d vector
    should change to Complex
*/
struct Offset {
public:

  Real coordinate_a_[NO_AXES];
    
  Real &y() { return coordinate_a_[Y_AXIS]; }
  Real &x() { return coordinate_a_[X_AXIS]; }
  Real y() const { return coordinate_a_[Y_AXIS]; }
  Real x() const { return coordinate_a_[X_AXIS]; }
    
  Real &operator[](Axis i) {
    return coordinate_a_[i];
  }
  Real operator[](Axis i) const{
    return coordinate_a_[i];
  }
    
  Offset& operator+=(Offset o) {
    x()+=o.x ();
    y()+=o.y ();
    return *this;
  }
  Offset operator - () const {
    Offset o = *this;
    o.x () = - o.x ();
    o.y () = - o.y ();
    return *this;
  }
  Offset& operator-=(Offset o) {
    x()-=o.x ();
    y()-=o.y ();
    return *this;
  }
  
  Offset &scale (Offset o) {
    x()*=o.x ();
    y()*=o.y ();
    return *this;
  }
  Offset &operator *=(Real a) {
    y() *= a;
    x() *= a;
    return *this;
  }
      
  Offset (Real ix , Real iy) {
    x()=ix;
    y()=iy;
  }
  Offset() {
    x()=0.0;
    y()=0.0;
  }
#ifndef STANDALONE
  String str () const;
#endif

  void mirror (Axis);
  Real  arg () const;
  Real length () const;
};

Offset complex_multiply (Offset, Offset);
Offset complex_exp (Offset);


inline Offset
operator* (Offset z1, Offset z2)
{
  return complex_multiply (z1,z2);
}

inline Offset
operator+ (Offset o1, Offset const& o2)
{
  o1 += o2;
  return o1;
}

inline Offset
operator- (Offset o1, Offset const& o2)
{
  o1 -= o2;
  return o1;
}


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


