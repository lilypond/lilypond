/*
  molecule.hh -- declare Molecule

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#ifndef MOLECULE_HH
#define MOLECULE_HH

#include "lily-proto.hh"
#include "plist.hh"
#include "box.hh"
#include "axes.hh"
#include "direction.hh"
#include "protected-scm.hh"
#include "cons.hh"

/** a group of individually translated symbols. You can add molecules
    to the top, to the right, etc.  */
class Molecule {
  //  Protected_scm atom_list_;	// change to List<Atom>?
  Killing_cons<Atom> *atom_list_;
  friend class Paper_outputter;
public:
  Box dim_;

  Molecule();
  ~Molecule();

  void add_at_edge (Axis a, Direction d, const Molecule &m, Real padding);
  
  void add_molecule (Molecule const &m);
  void translate (Offset);
  void do_center (Axis);
  void translate_axis (Real,Axis);
  
  void add_atom (Atom const *a);
  /// how big is #this#? 
  Box extent() const;
  Interval extent (Axis) const;

  Molecule (const Molecule&s);
  void print() const;
  void operator=(const Molecule&);  
};
#endif
