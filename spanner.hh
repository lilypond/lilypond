/*
  spanner.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SPANNER_HH
#define SPANNER_HH
#include "proto.hh"

/// a symbol which is attached between two columns.
struct Spanner {
    const PCol *left, *right;
    Parametric_symbol *strets;
    PStaff * pstaff_;
    /// clone a piece of  this spanner.
    virtual Spanner *broken_at(const PCol *c1, const PCol *c2) const; 
    /**
 
    PRE
    c1 >= start, c2  <= stop
    */
    /****************/
    String TeXstring () const ;
    Spanner();
    virtual void process();
};
/** Spanner should know about the items which it should consider:
    e.g. slurs should be steep enough to "enclose" all those items. This
    is absolutely necessary for beams, since they have to adjust the
    length of stems of notes they encompass.

    */
#endif
