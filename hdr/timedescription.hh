/*
  moment.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef tdes_HH
#define tdes_HH

#include "real.hh"
#include "moment.hh"

/// full info on where we are
struct Time_description {
    Moment when;

    /// if true, no bars needed, no reduction of whole_in_measure
    bool cadenza_b_;
    
    /// current measure info
    Moment whole_per_measure;

    /// where am i 
    Moment whole_in_measure;

    /// how long is one beat?
    Moment one_beat;

    /// idem
    int bars;

    /****************/
    void set_cadenza(bool);
    void OK() const;
    Time_description(Moment, const Time_description*);
    String str()const;
    void print() const;
    void setpartial(Moment p);
    Moment barleft();
    void set_meter(int,int);
    static int compare (Time_description&, Time_description&);
};

#include "compare.hh"


instantiate_compare(Time_description&,Time_description::compare);

#endif // Time_description_HH

