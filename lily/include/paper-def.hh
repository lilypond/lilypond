/*
  paper-def.hh -- declare Paper_def

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef PAPER_DEF_HH
#define PAPER_DEF_HH
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
    /**
      The distance between lines
     */
    Real interline()const;
    /// half the distance between lines
    Real internote()const;

    /// thickness of the standard line 
    Real rule_thickness()const;

    /// height of the staff
    Real standard_height()const;

    /// width of a quaver ball
    Real note_width() const;
    void print() const;
    Lookup const * lookup_l();	// TODO naming

    /** convert a duration to an idealspacing
      influence using the geometric_ and whole_width parameters.
      */
    Real duration_to_dist(Moment);
};

#endif // Paper_def_HH

