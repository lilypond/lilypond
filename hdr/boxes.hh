/*
    some 2D geometrical concepts
*/

#ifndef BOXES_HH
#define BOXES_HH

#include "fproto.hh"
#include "real.hh"
#include "interval.hh"


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


/// a 4-tuple of #Real#s
struct Box {
    Interval x, y;
    
    void translate(Offset o) {
	x.translate(o.x);
	y.translate(o.y);
    }
    void unite(Box b) {
	x.unite(b.x);
	y.unite(b.y);
    }
    Box(svec<Real> &);
    Box();
    Box(Interval ix, Interval iy);
};


#endif
