#include "proto.hh"
#include "real.hh"
#include "string.hh"
#include "moment.hh"


/** symbols, dimensions and constants

  This struct takes care of all kinds of symbols, dimensions and
 constants. Most of them are related to the point-size of the fonts,
 so therefore, the lookup table for symbols is also in here.

 see TODO
 */
struct Paperdef {
    Lookup *lookup_p_;
    String outfile;

    Real linewidth;

    /// how much space does a whole note take (ideally?)
    Real whole_width;

    /// ideal = geometric_ ^ log2(duration)
    Real geometric_;
    
    /* *************** */
    void reinit();
    Paperdef(Lookup*);
    void set(Lookup*);
    ~Paperdef();
    Paperdef(Paperdef const&);
    Real interline()const;
    Real internote()const;
    Real rule_thickness()const;
    Real standard_height()const;
    Real note_width() const;
    void print() const;
    Real duration_to_dist(Moment);
};

