/*
  spanner.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SPANNER_HH
#define SPANNER_HH

#include "proto.hh"
#include "staffelem.hh"

/// a symbol which is attached between two columns.
struct Spanner:Staff_elem {
    PCol *left, *right;

    
    /****************/
    
    Spanner();
    virtual Interval width()const;
    void print()const;

    Spanner *broken_at(PCol *c1,  PCol *c2) const;
protected:
    /// clone a piece of  this spanner.
    virtual Spanner *do_break_at( PCol *c1,  PCol *c2) const=0; 
    /**
 
    PRE
    c1 >= start, c2  <= stop
    */
};
/**
  A spanner is a symbol whose final appearance can only be calculated
  after the breaking problem is solved.

  Examples

  - (de)crescendo
  - slur
  - beam
  - bracket
  

  Spanner should know about the items which it should consider:
    e.g. slurs should be steep enough to "enclose" all those items. This
    is absolutely necessary for beams, since they have to adjust the
    length of stems of notes they encompass.

    */
#endif
