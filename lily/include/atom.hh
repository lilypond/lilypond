/*
  atom.hh -- declare Atom

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef ATOM_HH
#define ATOM_HH

#include "string.hh"
#include "boxes.hh"
#include "lily-proto.hh"


/// a symbol which can be translated, and freely copied
struct Atom {
  String tex_;
  Box dim_;
  Offset off_;

  String str() const;		// for printing.
  Atom (String, Box);
  Atom ();
  void translate (Offset o);
  void translate (Real r,Axis a);
  /// how big is #this#?
  Box extent() const;
  void print() const;
  String TeX_string() const;
};
#endif
