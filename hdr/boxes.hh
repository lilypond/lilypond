/*
    some 2D geometrical concepts
*/

#ifndef BOXES_HH
#define BOXES_HH

#include "fproto.hh"
#include "real.hh"
#include "interval.hh"
#include "offset.hh"

/// a square subset of Real^2
struct Box {
    Interval x, y;

    void translate(Offset o) {
	x.translate(o.x);
	y.translate(o.y);
    }
    /// smallest box enclosing #b#
    void unite(Box b) {
	x.unite(b.x);
	y.unite(b.y);
    }
    Box();
    Box(Interval ix, Interval iy);
};


#endif
