/*
  moment.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef MOMENT_HH
#define MOMENT_HH

#include "real.hh"


struct Moment {
    Real when;
    /// current measure info
    Real whole_per_measure;

    /// where am i 
    Real whole_in_measure;

    /// how long is one beat?
    Real one_beat;

    /// idem
    int bars;
    /****************/
    void OK() const;
    Moment(Real, const Moment*);
    void print() const;
    void setpartial(Real p);
    Real barleft();
    void set_meter(int,int);
};

#endif // MOMENT_HH

