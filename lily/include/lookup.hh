/*
  lookup.hh -- declare Lookup

  source file of the GNU LilyPond music typesetter

  (c) 1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LOOKUP_HH
#define LOOKUP_HH

#include "molecule.hh"
#include "fproto.hh"
#include "direction.hh"
#include "box.hh"

/** handy interface to symbol table
 */
class Lookup
{
public:
  Lookup ();
  Lookup (Lookup const&);


  
  Molecule special_ball (int, String) const;
  Molecule simple_bar (String s, Real w) const;
  Molecule accidental (int, bool cautionary) const;
  Molecule afm_find (String, bool warn=true) const;
  Molecule ball (int) const;
  Molecule bar (String, Real height) const;
  Molecule beam (Real, Real, Real) const;
  Molecule clef (String) const;
  Molecule dashed_slur (Array<Offset> controls, Real thick, Real dash) const;
  Molecule dots () const;
  Molecule ledger_line (Interval) const;
  Molecule fill (Box b) const;
  Molecule filledbox (Box b) const;  
  Molecule flag (int, Direction) const;
  Molecule hairpin (Real width, bool decresc, bool continued) const;
  Molecule plet (Real dy, Real dx, Direction dir) const;
  Molecule rest (int, bool outside) const;
  Molecule rule_symbol (Real height, Real width) const;
  Molecule script (String idx) const;
  Molecule stem (Real y1_pos, Real y2_pos) const;
  Molecule slur (Array<Offset> controls) const;
  Molecule streepje (int type) const;
  Molecule text (String style, String text) const;
  Molecule staff_brace (Real dy) const;
  Molecule staff_bracket (Real dy) const;
  Molecule volta (Real w, bool last_b) const;
  Molecule special_time_signature (String, int,int) const;
  Molecule time_signature (int n,int d) const;

  Paper_def * paper_l_;

  String font_name_;
  Adobe_font_metric * afm_l_;
};

#endif // LOOKUP_HH
