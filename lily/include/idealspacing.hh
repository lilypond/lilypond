/*
  idealspacing.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef IDEALSPACING_HH
#define IDEALSPACING_HH
#include "proto.hh"

/// ideal spacing between two columns
struct Idealspacing {

    /// the ideal distance
    Real space;

    /// Hooke's constant: how strong are the "springs" attached to columns
    Real hooke;

    /// the two columns
    PCol const *left, *right;
    
    void print()const;
    void OK() const ;
    Idealspacing(PCol const *left,PCol const *right);    
};


#endif // IDEALSPACING_HH

