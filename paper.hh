#include "proto.hh"
#include "real.hh"
#include "string.hh"

struct Paperdef {
    Lookup *lookup_;
    String outfile;
    Real linewidth;
    /// how much space does a whole note take (ideally?)
    Real whole_width;
    
    /****************/
    void parse();
    Paperdef();
    ~Paperdef();
    Real interline()const;
    Real standard_height()const;
    Real note_width() const;
    void print() const;
};

