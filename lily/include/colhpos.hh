/*
  colhpos.hh -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef COLHPOS_HH
#define COLHPOS_HH
#include "varray.hh"
#include "proto.hh"

typedef Array<PCol*>  Line_of_cols;

struct Col_hpositions {
    bool ugh_b_;
    Line_of_cols error_col_l_arr_;
    Line_of_cols cols;
    Array<Real> config;
    Real energy;

    /* ************** */
    void OK()const;

    Col_hpositions();
    void add( PCol*c);
    void print() const;
};


#endif // COLHPOS_HH

