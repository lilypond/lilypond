/*
  lookup.hh -- declare Lookup

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef LOOKUPSYMS_HH
#define LOOKUPSYMS_HH

#include "atom.hh"
#include "fproto.hh"
#include "scalar.hh"
#include "direction.hh"

/** handy interface to symbol table
 */
struct Lookup {
    Paper_def * paper_l_;
    Symtables *symtables_;
    String texsetting;
    /* *************** */
    void add (String, Symtable*);
    void print() const;

    Atom linestaff (int n, Real ss, Real w) const;
    Atom fill (Box b) const;
    Atom beam_element (int,int,Real=0) const;

    /// round slope to closest TeXslope
    Atom beam (Real&,Real) const;

    /**
      pos == 3 : 3 lines above staff (extending below note)

      pos == -3: below staff
      */
    Atom streepjes (int type, int pos) const;

    Atom vbrace (Real &dy) const;
    Atom meter (Array<Scalar>) const;
    Atom stem (Real y1_pos, Real y2_pos) const;
    Atom rule_symbol (Real height, Real width) const;
    Atom accidental (int) const;
    Atom ball (int) const;
    Atom flag (int, Direction) const;
    Atom rest (int, bool outside) const;
    Atom clef (String) const;
    Atom bar (String, Real height) const;
    
    Atom dots () const;
    Atom slur (int dy, Real &dx, Direction dir) const;
    Atom half_slur (int dy, Real &dx, Direction dir, int xpart) const;
    Atom half_slur_middlepart (Real &dx, Direction dir) const;
    Atom big_slur (int dy, Real &dx, Direction dir) const;
    Atom text (String style, String text, int align = 1) const;
    Atom script (String idx) const;
    Atom hairpin (Real & width, bool decresc) const;
    Atom dynamic (String) const;
    Lookup();
    Lookup (Lookup const &);
    ~Lookup();
};

#endif
