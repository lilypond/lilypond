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

    Symbol beam_element(int,int,Real=0);
    /// round slope to closest TeXslope
    Symbol beam(Real&,Real);
    Symbol streepjes(int pos);
    /**
      pos == 3 : 3 lines above staff (extending below note)

      pos == -3: below staff
      */

    Symbol rule_symbol(Real height, Real width);
    Symbol accidental(int);
    Symbol ball(int);
    Symbol flag(int);
    Symbol rest(int);
    Symbol clef(String);
    Symbol bar(String);
    Symbol dots(int);
    Lookup();
    ~Lookup();
};

#endif
