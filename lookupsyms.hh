/*
  lilypond, (c) 1996 Han-Wen Nienhuys
*/
#ifndef LOOKUPSYMS_HH
#define LOOKUPSYMS_HH

#include "symbol.hh"

struct Lookup {
    Symtables *symtables_;
    
    /****************/

    void parse (Text_db&t);
    Parametric_symbol *linestaff(int n);
    Parametric_symbol *meter(String);
    Parametric_symbol *stem();
    Symbol ball(int);
    Symbol flag(int);
    Symbol rest(int);
    Symbol bar(String);
    Symbol dots(int);
    Lookup();
    ~Lookup();
};

#endif
