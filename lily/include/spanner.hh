/*
  spanner.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SPANNER_HH
#define SPANNER_HH

#include "proto.hh"
#include "staff-elem.hh"


/** a symbol which is attached between two columns. A spanner is a
  symbol which spans across several columns, so its final appearance
  can only be calculated after the breaking problem is solved.

  Examples

  \begin{itemize}
  \item (de)crescendo
  \item slur
  \item beam
  \item bracket
  \end{itemize}
  

  Spanner should know about the items which it should consider:
  e.g. slurs should be steep enough to "enclose" all those items. This
  is absolutely necessary for beams, since they have to adjust the
  length of stems of notes they encompass.

    */
class Spanner:public Staff_elem {
public:
    PCol *left_col_l_, *right_col_l_;

    
    /* *************** */
    NAME_MEMBERS(Spanner);
    virtual Spanner* spanner() { return this; }    
    Spanner();
    Spanner *broken_at(PCol *c1,  PCol *c2) const;
protected:

    virtual Interval do_width()const;
    void do_print()const;


    /**
      clone a piece of  this spanner.
      PRE
      c1 >= start, c2  <= stop
    */
    virtual Spanner *do_break_at( PCol *c1,  PCol *c2) const=0;
};
#endif
