/*
  scoreline.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SCORELINE_HH
#define SCORELINE_HH
#include "plist.hh"
#include "proto.hh"
#include "vray.hh"

/// the columns of a score that form one line.
struct
Line_of_score {
    PointerList<const PCol *> cols;

    // need to store height of each staff.
    IPointerList<Line_of_staff*> staffs;
    PScore * pscore_;	// needed to generate staffs

    /****************/
    
    Line_of_score(svec<const PCol *> sv,  PScore *);

    String TeXstring() const;

    // is #c# contained in #*this#?
    bool element(const PCol *c);
};

#endif

