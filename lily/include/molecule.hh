/*
  molecule.hh -- declare Molecule

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#ifndef MOLECULE_HH
#define MOLECULE_HH

#include "lily-proto.hh"
#include "plist.hh"
#include "boxes.hh"
#include "axes.hh"
#include "direction.hh"

/** a group of individually translated symbols. You can add molecules
    to the top, to the right, etc.  */
struct Molecule {
  Pointer_list<Atom*> ats;	// change to List<Atom>? 

  /* *************** */
    
  Molecule() { }
  Molecule (Atom const &a) { add (a) ;}

  void add_at_edge (Axis a, Direction d, const Molecule &m);
  
  void add (Molecule const &m);
  void translate (Offset);
  void translate_axis (Real,Axis);
  void add (Atom const & a) ;
  /// how big is #this#? 
  Box extent() const;

  String TeX_string() const;

  Molecule (const Molecule&s);
  void print() const;
private:
  void operator=(const Molecule&);
};
#endif
