/*
  offset.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef OFFSET_HH
#define OFFSET_HH
#include "real.hh"

/// 2d vector 
struct Offset {
    Real x,y;

    Offset operator+(Offset o)const {
	Offset r(*this);
	r+=o;
	return r;
    }
    
    Offset operator+=(Offset o) {
	x+=o.x;
	y+=o.y;
	return *this;
    }
    Offset(Real ix , Real iy) {
	x=ix;
	y=iy;
    }
    Offset() {
	x=0.0;
	y=0.0;
    }
};

#endif // OFFSET_HH


