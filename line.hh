#ifndef LINE_HH
#define LINE_HH

/*
    horizontal structures for broken scores.
*/

#include "real.hh"
#include "list.hh"
#include "vray.hh"
#include "glob.hh"
#include "pstaff.hh"



/// the columns of a score that form one line.
struct
Line_of_score {
    List<const PCol *> cols;

    // need to store height of each staff.
    PointerList<Line_of_staff*> staffs;
    const PScore * score;	// needed to generate staffs

    /****************/
    
    Line_of_score(svec<const PCol *> sv, const PScore *);

    String TeXstring() const;

    // is #c# contained in #*this#?
    bool element(const PCol *c);
};

/// one broken line of staff.
struct Line_of_staff {
    Real height;

    /// y-pos of the baseline, measured from the top.
    Real base;
    
    PointerList<Spanner *> brokenspans;    
    Line_of_score const * scor;
    const PStaff *pstaff_;

    /****************/
    
    String TeXstring() const;
    Line_of_staff(Line_of_score*, PStaff *);
    Real maxheight()const;
};

#endif
