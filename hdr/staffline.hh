/*
  staffline.hh --     horizontal structures for broken scores.

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef STAFFLINE_HH
#define STAFFLINE_HH

#include "real.hh"
#include "plist.hh"
#include "varray.hh"
#include "glob.hh"
#include "pstaff.hh"

/// one broken line of staff.
struct Line_of_staff {

    Line_of_score  * line_of_score_l_;
    PStaff *pstaff_l_;

    /****************/
    
    String TeXstring() const;
    Line_of_staff(Line_of_score*, PStaff *);
    Interval height() const;
    void process();
};

#endif
