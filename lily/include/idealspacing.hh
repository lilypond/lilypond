/*
  idealspacing.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef IDEALSPACING_HH
#define IDEALSPACING_HH
#include "lily-proto.hh"
#include "drul-array.hh"

/// ideal spacing between two columns
struct Idealspacing {

  /// the ideal distance
  Real space_f_;

  /// Hooke's constant: how strong are the "springs" attached to columns
  Real hooke_f_;

  /// the two columns
  Drul_array<int> cols_drul_;
    
  Idealspacing();
};


#endif // IDEALSPACING_HH

