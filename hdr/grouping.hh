/*
  grouping.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef GROUPING_HH
#define GROUPING_HH

#include "moment.hh"
#include "interval.hh"
#include "varray.hh"

typedef Interval_t<Moment> MInterval;

/// data structure which represents rhythmic units 
struct Rhythmic_grouping {
    Array<Rhythmic_grouping*> children;
    MInterval *interval_;
    
    /****************/

    Array<MInterval> intervals();
    MInterval interval()const;
    Moment length() const;
    void intersect(MInterval);
    
    void operator=(Rhythmic_grouping const&);
    Rhythmic_grouping(Rhythmic_grouping const&);
    Rhythmic_grouping(MInterval, int n=1);
    Rhythmic_grouping();
    Rhythmic_grouping(Array<Rhythmic_grouping*>);
    ~Rhythmic_grouping();

    void add_child(Moment start, Moment len);
    bool child_fit_query(Moment start);
    void split(Rhythmic_grouping r);
    void split(Array<MInterval>);
    void split(int n);

    void print() const;
    void OK() const;

    Array<int> generate_beams(Array<int>, int&);

    /// multiply self to span #i#
    void extend(MInterval i);
    void translate(Moment);
private:
    void init();
    void junk();
    void copy(Rhythmic_grouping const&);
};
/**
  this is a tree. It groupes notes according to rules
  
 */

#endif
