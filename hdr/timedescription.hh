/*
  moment.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef tdes_HH
#define tdes_HH

#include "real.hh"
#include "moment.hh"

struct Time_description {
    Rational when;
    /// current measure info
    Rational whole_per_measure;

    /// where am i 
    Rational whole_in_measure;

    /// how long is one beat?
    Rational one_beat;

    /// idem
    int bars;
    /****************/
    void OK() const;
    Time_description(Rational, const Time_description*);
    void print() const;
    void setpartial(Rational p);
    Rational barleft();
    void set_meter(int,int);
};

#endif // Time_description_HH

