#ifndef TEX_HH
#define TEX_HH
#include "string.hh"
#include "boxes.hh"

struct Symbol {
    String tex;
    Box dim;

    Symbol (String, Box );
    static const Symbol*find_ball(int);
    static const Symbol*find_rest(int);
    static const Symbol*find_bar(String);
    Symbol() { }
};

/// a symbol with a variable width
struct Stretchable_symbol {
public:

    /// return a string for a symbol in this width.
    virtual String operator ()(Real width)=0;

    static const Stretchable_symbol* get_linestaff(int n);
};

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
