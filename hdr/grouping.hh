/*
  grouping.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef GROUPING_HH
#define GROUPING_HH

#include "moment.hh"
#include "interval.hh"
#include "vray.hh"

typedef Interval_t<Moment> MInterval;

/// data structure which represents rhythmic units 
struct Rhythmic_grouping {    
    
    svec<Rhythmic_grouping*> children;
    MInterval *interval_;
    
    /****************/

    svec<MInterval> intervals();
    MInterval interval()const;
    Moment length() const;
    void intersect(MInterval);
    
    void operator=(Rhythmic_grouping const&);
    Rhythmic_grouping(Rhythmic_grouping const&);
    Rhythmic_grouping(MInterval, int n=1);
    Rhythmic_grouping();
    Rhythmic_grouping(svec<Rhythmic_grouping*>);
    ~Rhythmic_grouping();

    void add_child(Moment start, Moment len);

    void split(Rhythmic_grouping r);
    void split(svec<MInterval>);
    void split(int n);

    void print() const;
    void OK() const;

    svec<int> generate_beams(svec<int>, int&);

private:
    void init();
    void junk();
    void copy(Rhythmic_grouping const&);
};
/**
  this is a tree. It groupes notes according to rules
  
 */

#endif
