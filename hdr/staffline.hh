/*
  staffline.hh --     horizontal structures for broken scores.

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef STAFFLINE_HH
#define STAFFLINE_HH

#include "real.hh"
#include "plist.hh"
#include "vray.hh"
#include "glob.hh"
#include "pstaff.hh"

/// one broken line of staff.
struct Line_of_staff {
    IPointerList<Spanner *> brokenspans;    
    Line_of_score const * scor;
    const PStaff *pstaff_;

    /****************/
    
    String TeXstring() const;
    Line_of_staff(Line_of_score*, PStaff *);
    Interval height() const;
};

#endif
