/*
  sccol.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SCCOL_HH
#define SCCOL_HH
#include "proto.hh"
#include "varray.hh"
#include "moment.hh"


/**

    When typesetting hasn't started on PScore yet, the columns which
    contain data have a rhythmical position. Score_column is the type
    with a rhythmical time attached to it. The calculation of
    idealspacing is done with data in these columns. (notably: the
    #durations# field)

    */

class Score_column {
    friend class Score;
    friend class Score_walker;

    bool musical_b_;
    Moment when_;
    void set_breakable();
public:
    /// indirection to column
    PCol * pcol_l_;

    /// length of notes/rests in this column
    Array<Moment> durations;
    
    /* *************** */

    Moment when() {  return when_; }
    Score_column(Moment when);       
    static int compare(Score_column & c1, Score_column &c2);
    void add_duration(Moment );
    void preprocess();
    bool breakable_b();
    bool musical_b() { return musical_b_; }
    bool used_b();
    void print() const;


};

#include "compare.hh"

instantiate_compare(Score_column&, Score_column::compare);

#endif // SCCOL_HH




