/*
  sccol.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SCCOL_HH
#define SCCOL_HH
#include "pcol.hh"


struct Score_column {
    PCol * pcol;
    svec<Real> durations;
    Real when;

    /// 
    bool musical;
    

    Score_column(Real when);

    static int compare(Score_column & c1, Score_column &c2) {
	return sgn(c1.when - c2.when);
    }
    void set_breakable() {
	 pcol->set_breakable();
    }
    bool used();
    void print() const;
};
/**

    When typesetting hasn't started on PScore yet, the columns which
    contain data have a rhythmical position. Score_column is the type
    with a rhythmical time attached to it. The calculation of
    idealspacing is done with data in these columns. (notably: the
    #durations# field)

    */

instantiate_compare(Score_column&, Score_column::compare);

#endif // SCCOL_HH

