/*
  spanner.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SPANNER_HH
#define SPANNER_HH

#include "proto.hh"
#include "interval.hh"

/// a symbol which is attached between two columns.
struct Spanner {
    PCol *left, *right;
    PStaff * pstaff_;
    Molecule *output ;

    /****************/

    String TeXstring () const ;
    Spanner();
    Paperdef *paper() const;
    virtual ~Spanner();
    virtual Interval height()const=0;
    /**
      PRE:
      processed
      */
  /// do calcs
    virtual void calculate();
    /**
      It is safe to call calculate multiple times on one object
      */
   virtual Interval width()const;
    virtual void process();
    virtual void preprocess();


    /// clone a piece of  this spanner.
    virtual Spanner *broken_at( PCol *c1,  PCol *c2) const=0; 
    /**
 
    PRE
    c1 >= start, c2  <= stop
    */
    virtual void print() const;
};
/**
  A spanner is a symbol whose appearance can only be calculated after the breaking problem is solved.

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
