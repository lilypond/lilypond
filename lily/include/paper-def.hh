/*
  paper-def.hh -- declare 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef Paper_def_HH
#define Paper_def_HH
#include "proto.hh"
#include "real.hh"
#include "string.hh"
#include "moment.hh"


/** symbols, dimensions and constants

  This struct takes care of all kinds of symbols, dimensions and
 constants. Most of them are related to the point-size of the fonts,
 so therefore, the lookup table for symbols is also in here.

 */
class Paper_def {
    Lookup *lookup_p_;
public:    
    String outfile;

    Real linewidth;

    /// how much space does a whole note take (ideally?)
    Real whole_width;

    /// ideal = geometric_ ^ log2(duration)
    Real geometric_;
    
    /* *************** */
    void reinit();
    Paper_def(Lookup*);
    void set(Lookup*);
    ~Paper_def();
    Paper_def(Paper_def const&);
    Real interline()const;
    Real internote()const;
    Real rule_thickness()const;
    Real standard_height()const;
    Real note_width() const;
    void print() const;
    const Lookup* lookup_l();	// TODO naming
    Real duration_to_dist(Moment);
};

#endif // Paper_def_HH

