#ifndef SYMBOL_HH
#define SYMBOL_HH
#include "string.hh"
#include "boxes.hh"
struct Symbol {
    String tex;
    Box dim;

    Symbol (String, Box );
    static const Symbol*find_ball(int);
    static const Symbol*find_rest(int);
    static const Symbol*find_bar(String);
    Symbol() ;
};

/// a symbol with a variable width
struct Stretchable_symbol {
public:

    /// return a string for a symbol in this width.
    virtual String operator ()(Real width)=0;
    virtual Interval height(Real width) const =0;
    static const Stretchable_symbol* get_linestaff(int n);
};

#endif
