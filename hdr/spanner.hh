/*
  spanner.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SPANNER_HH
#define SPANNER_HH

#include "proto.hh"
#include "staffelem.hh"


/** a symbol which is attached between two columns. A spanner is a symbol which spans across several columns, so its
  final appearance can only be calculated after the breaking problem
  is solved.

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
struct Spanner:Staff_elem {
    PCol *left, *right;

    
    /* *************** */
    
    Spanner();
    virtual Interval width()const;
    void do_print()const;
    const char* name()const;
    Spanner *broken_at(PCol *c1,  PCol *c2) const;
    virtual Spanner* spanner() { return this; }
protected:

    /**
  clone a piece of  this spanner.
    PRE
    c1 >= start, c2  <= stop
    */
    virtual Spanner *do_break_at( PCol *c1,  PCol *c2) const=0; 
};
#endif
