/*
  idealspacing.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
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
    const PCol *left, *right;
    
    void print()const;
    void OK() const ;
    Idealspacing(const PCol *left,const PCol *right);    
};


#endif // IDEALSPACING_HH

