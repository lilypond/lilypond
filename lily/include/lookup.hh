/*
  lookup.hh -- declare Lookup

  source file of the GNU LilyPond music typesetter

  (c) 1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LOOKUP_HH
#define LOOKUP_HH

#include "atom.hh"
#include "molecule.hh"
#include "fproto.hh"
#include "scalar.hh"
#include "direction.hh"
#include "curve.hh"
#include "afm.hh"
#include "symtable.hh"
#include "box.hh"

/** handy interface to symbol table
 */
class Lookup
{
public:
  Lookup ();
  Lookup (Lookup const&);
  Lookup (Symtables const&);
  ~Lookup ();
  
  Molecule accidental (int, bool cautionary) const;
  void add (String, Symtable*);
  Atom afm_find (String, bool warn=true) const;
  Atom ball (int) const;
  Atom bar (String, Real height) const;
  Atom beam (Real, Real, Real) const;
  Atom clef (String) const;
  Atom dashed_slur (Array<Offset> controls, Real thick, Real dash) const;
  Atom dots () const;
  Atom dynamic (String) const;
  Atom fill (Box b) const;
  Atom flag (int, Direction) const;
  Atom hairpin (Real width, bool decresc, bool continued) const;
  Atom plet (Real dy, Real dx, Direction dir) const;
  void print () const;
  Atom rest (int, bool outside) const;
  Atom rule_symbol (Real height, Real width) const;
  Atom script (String idx) const;
  Atom stem (Real y1_pos, Real y2_pos) const;
  Atom slur (Array<Offset> controls) const;
  Atom streepje (int type) const;
  Atom text (String style, String text) const;
  Atom vbrace (Real &dy) const;
  Atom vbracket (Real &dy) const;
  Atom special_time_signature (String, Array<int>) const;
  Atom time_signature (Array<int>) const;

  Paper_def * paper_l_;
  Symtables *symtables_p_;
  String font_;
  String font_path_;  
  Adobe_font_metric * afm_p_;
};

#endif // LOOKUP_HH
