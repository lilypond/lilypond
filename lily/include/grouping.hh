/*
  grouping.hh -- part of GNU LilyPond

  (c) 1996--1998 Han-Wen Nienhuys
*/

#ifndef GROUPING_HH
#define GROUPING_HH

#include "minterval.hh"
#include "array.hh"

/** data structure which represents rhythmic units   this is a tree. It groupes notes according to rules

  TODO Documentation. Unhairing
 */
struct Rhythmic_grouping {
    Array<Rhythmic_grouping*> children;
    MInterval *interval_;
    

    Array<MInterval> intervals();
    MInterval interval() const;
    Moment length() const;
    void intersect (MInterval);
    
    void operator=(Rhythmic_grouping const&);
    Rhythmic_grouping (Rhythmic_grouping const&);
    Rhythmic_grouping (MInterval, int n=1);
    Rhythmic_grouping();
    Rhythmic_grouping (Array<Rhythmic_grouping*>);
    ~Rhythmic_grouping();

    void add_child (Moment start, Moment len);
    bool child_fit_b (Moment start);
    void split (Rhythmic_grouping r);
    void split (Array<MInterval>);
    void split (int n);

    void print() const;
    void OK() const;

    Array<int> generate_beams (Array<int>, int&);

    /** multiply self to span #i#.
      In implementation, this isn't really const, but conceptually it is.
      */
    void extend (MInterval i) const;
    void translate (Moment);
private:
    void init();
    void junk();
    void copy (Rhythmic_grouping const&);
};


Rhythmic_grouping parse_grouping (Array<int> beat_i_arr, Array<Moment> elt_length_arr);


#endif
