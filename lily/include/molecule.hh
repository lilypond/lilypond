/*
  molecule.hh -- declare Molecule

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#ifndef MOLECULE_HH
#define MOLECULE_HH

#include "lily-proto.hh"
#include "plist.hh"
#include "box.hh"
#include "axes.hh"
#include "direction.hh"

/** a group of individually translated symbols. You can add molecules
    to the top, to the right, etc.  */
struct Molecule {
  Pointer_list<Atom*> atoms_;	// change to List<Atom>? 

    
  Molecule() { }
  Molecule (Atom const &a);

  void add_at_edge (Axis a, Direction d, const Molecule &m);
  
  void add_molecule (Molecule const &m);
  void translate (Offset);
  void translate_axis (Real,Axis);
  void add_atom (Atom const & a) ;
  /// how big is #this#? 
  Box extent() const;


  Molecule (const Molecule&s);
  void print() const;
private:
  void operator=(const Molecule&);
};
#endif
