/*
  lookup.hh -- declare Lookup

  source file of the GNU LilyPond music typesetter

  (c) 1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LOOKUP_HH
#define LOOKUP_HH

#include "lily-guile.hh"
#include "molecule.hh"
#include "fproto.hh"
#include "direction.hh"
#include "box.hh"

/**
   handy interface to symbol table
   TODO: move this into GUILE?
 */
class Lookup
{
public:
  Lookup ();
  Lookup (Lookup const&);

  Molecule simple_bar (String s, Real w, Paper_def*) const;
  Molecule afm_find (String, bool warn=true) const;
  Molecule bar (String, Real height, Paper_def*) const;
  Molecule beam (Real, Real, Real) const;
  Molecule dashed_slur (Array<Offset> controls, Real thick, Real dash) const;
  Molecule ledger_line (Interval) const;
  Molecule fill (Box b) const;
  Molecule filledbox (Box b) const;  
  Molecule tuplet_bracket (Real dy, Real dx, Real gap, Real thick,Real height, Direction dir) const;
  Molecule accordion (SCM arg, Real interline_f) const;
  Molecule slur (Array<Offset> controls, Real thick) const;
  Molecule text (String style, String text, Paper_def*) const;
  Molecule staff_brace (Real dy, int) const;
  Molecule staff_bracket (Real height, Paper_def* paper_l) const;
  Molecule volta (Real h, Real w,  Real thick,  bool vert_start, bool vert_end) const;
  Molecule special_time_signature (String, int,int, Paper_def*) const;
  Molecule time_signature (int n,int d, Paper_def*) const;

  String font_name_;
  Adobe_font_metric * afm_l_;
};

#endif // LOOKUP_HH
