/*
  grouping.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef GROUPING_HH
#define GROUPING_HH

#include "interval.hh"
#include "vray.hh"


/// data structure which represents rhythmic units 
struct Rhythmic_grouping {    
    
    svec<Rhythmic_grouping*> children;
    Interval *interval_;
    
    /****************/

    svec<Interval> intervals();
    Interval interval()const;
    Real length() const;
    void intersect(Interval);
    
    void operator=(Rhythmic_grouping const&);
    Rhythmic_grouping(Rhythmic_grouping const&);
    Rhythmic_grouping(Interval, int n=1);
    Rhythmic_grouping();
    Rhythmic_grouping(svec<Rhythmic_grouping*>);
    ~Rhythmic_grouping();

    void add_child(Real start, Real len);

    void split(Rhythmic_grouping r);
    void split(svec<Interval>);
    void split(int n);

    void print() const;
    void OK() const;

    svec<int> generate_beams(svec<int>, int&);

private:
    void junk();
    void copy(Rhythmic_grouping const&);
};
/**
  this is a tree. It groupes notes according to rules
  
 */

#endif
