/*
  grouping.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef GROUPING_HH
#define GROUPING_HH

#include "interval.hh"
#include "vray.hh"



struct Rhythmic_grouping {    
    svec<Real> divisions;
    svec<Rhythmic_grouping*> children;

    /****************/

    svec<Real> interior();
    Rhythmic_grouping partial_grouping(Interval t);
    Real length() const;
    Interval time() const;
    Rhythmic_grouping(Interval);
    Rhythmic_grouping();
    Rhythmic_grouping(svec<Real>);
    Rhythmic_grouping(Rhythmic_grouping const&);
    
    void split(Rhythmic_grouping r);
    void split(svec<Real>);
    void intersect(Interval);
    void split(int n);
    void print() const;
    void OK() const;
    ~Rhythmic_grouping();
};
#endif
