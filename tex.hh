#ifndef TEX_HH
#define TEX_HH

#include "string.hh"
#include "boxes.hh"

/// anything which can be output
struct Output {
    virtual String TeXstring() const=0;
    /** generate a TeX string, which typesets the symbol. Vertical
     base position is the "origin" of the staff
    */
    virtual Box extent() const=0;
};
/**
  any output should (at least) be outputtable for TeX, and have a
  dimension
*/


/// an idea
struct Text_gob : Output {
    String text;
    // fonts, sizes, etc?
    virtual String TeXstring() const;
    virtual Box extent() const;
};


/// #h# is in points
String vstrut(Real h);


#endif
