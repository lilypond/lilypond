/*
  lilypond, (c) 1996,97 Han-Wen Nienhuys
*/
#ifndef LOOKUPSYMS_HH
#define LOOKUPSYMS_HH

#include "symbol.hh"
#include "fproto.hh"
#include "scalar.hh"

/// intuitive interface to symbol table
struct Lookup {
    Symtables *symtables_;
    String texsetting;
    /* *************** */
    void add(String, Symtable*);

    Real internote();

    Symbol linestaff(int n, Real w);
    Symbol fill(Box b);
    Symbol beam_element(int,int,Real=0);

    /// round slope to closest TeXslope
    Symbol beam(Real&,Real);

    /**
      pos == 3 : 3 lines above staff (extending below note)

      pos == -3: below staff
      */
    Symbol streepjes(int pos);

    Symbol meter(Array<Scalar>);
    Symbol stem(Real y1_pos, Real y2_pos);
    Symbol rule_symbol(Real height, Real width);
    Symbol accidental(int);
    Symbol ball(int);
    Symbol flag(int);
    Symbol rest(int);
    Symbol clef(String);
    Symbol bar(String);
    Symbol dots(int);
    Symbol slur(int dy, Real &dx, int dir);
    Symbol half_slur(int dy, Real &dx, int dir, int xpart);
    Symbol half_slur_middlepart(Real &dx, int dir);
    Symbol big_slur(int dy, Real &dx, int dir);
    Symbol text(String style, String text, int align = 1);
    Symbol script(String idx);
    Symbol hairpin(Real & width, bool decresc);

    Lookup();
    Lookup(Lookup const &);
    ~Lookup();
};

#endif
