/*
  vertical-spanner.hh -- declare Vertical_spanner

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef VERTICAL_SPANNER_HH
#define VERTICAL_SPANNER_HH

#include "staff-elem.hh"

class Vertical_spanner: virtual public Score_elem {
public:
    PStaff *lower_pstaff_l_;
    PStaff *upper_pstaff_l_;
    NAME_MEMBERS(Vertical_spanner);
    
    Vertical_spanner();
    
};
#endif // VERTICAL_SPANNER_HH
