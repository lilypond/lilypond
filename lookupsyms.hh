/*
  lilypond, (c) 1996 Han-Wen Nienhuys
*/
#ifndef LOOKUPSYMS_HH
#define LOOKUPSYMS_HH

#include "symbol.hh"

struct Lookup {
    static Parametric_symbol *linestaff(int n);
    static Parametric_symbol *meter(String);
    static Symbol ball(int);
    static Symbol rest(int);
    static Symbol bar(String);
    static Symbol dots(int);
};

#endif
