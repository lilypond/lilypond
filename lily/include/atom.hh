/*
  atom.hh -- declare Atom

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef ATOM_HH
#define ATOM_HH

#include "string.hh"
#include "box.hh"
#include "lily-proto.hh"


/// a symbol which can be translated, and freely copied
class Atom {
  Offset off_;
public:
  String str_;
  String font_;
  Box dim_;

  Offset offset () const;
  String str() const;		// for printing.
  Atom (String, Box);
  Atom ();
  void translate (Offset o);
  void translate_axis (Real r,Axis a);
  /// how big is #this#?
  Box extent() const;
  void print() const;
  bool check_infinity_b () const;
  bool empty() const;
};
#endif
