/*
  offset.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef OFFSET_HH
#define OFFSET_HH

#include "real.hh"
#include "axes.hh"


/// 2d vector 
struct Offset {
    Real coordinate_a_[NO_AXES];
    
    Real &y() { return coordinate_a_[Y_AXIS]; }
    Real &x() { return coordinate_a_[X_AXIS]; }
    Real y()const { return coordinate_a_[Y_AXIS]; }
    Real x()const { return coordinate_a_[X_AXIS]; }
    
    Real &operator[](Axis i) {
	return coordinate_a_[i];
    }
    Real operator[](Axis i) const{
	return coordinate_a_[i];
    }
    
    Offset operator+=(Offset o) {
	x()+=o.x ();
	y()+=o.y ();
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
};

inline Offset
operator+(Offset o1, Offset const& o2)
{
    o1 += o2;
    return o1;
}
    
#endif // OFFSET_HH


