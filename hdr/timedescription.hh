/*
  moment.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef tdes_HH
#define tdes_HH

#include "moment.hh"

/// full info on where we are
struct Time_description {
    Moment when_;

    /// if true, no bars needed, no reduction of whole_in_measure
    bool cadenza_b_;
    
    /// current measure info
    Moment whole_per_measure_;

    /// where am i 
    Moment whole_in_measure_;

    /// how long is one beat?
    Moment one_beat_;

    /// idem
    int bars_i_;

    /* *************** */
    void set_cadenza(bool);
    void OK() const;
    Time_description();
    void add(Moment dt);
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

