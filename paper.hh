#include "proto.hh"
#include "real.hh"
#include "string.hh"

struct Paperdef {
    Lookup *lookup_;
    String outfile;
    Real linewidth;
    /// how much space does a whole note take (ideally?)
    Real whole_width;

    // ideal = geometric_ ^ log2(duration)
    Real geometric_;
    
    /****************/
    void parse();
    Paperdef();
    ~Paperdef();
    Real interline()const;
    Real standard_height()const;
    Real note_width() const;
    void print() const;
    Real duration_to_dist(Real);
};

