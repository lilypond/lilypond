/*
  grouping.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef GROUPING_HH
#define GROUPING_HH

#include "interval.hh"
#include "vray.hh"

struct Rhythmic_grouping {
    Interval t;

    svec<Rhythmic_grouping*> children;
    /****************/
    
    void split_half();
    Real last();
    Rhythmic_grouping* sub_grouping(Interval v);
    void split_grouping(Rhythmic_grouping &initial_grouping);
    void split_grouping(svec<Real> initial_grouping);
    svec<Real> get_bounds();
    Rhythmic_grouping(Interval);
    Rhythmic_grouping(svec<Interval> notes,
		      svec<Real> initial_grouping);

    void print() const;
    ~Rhythmic_grouping();
};

#endif // GROUPING_HH
