/*
  molecule.hh -- declare Molecule

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#ifndef MOLECULE_HH
#define MOLECULE_HH

#include "lily-proto.hh"
#include "box.hh"
#include "axes.hh"
#include "direction.hh"
#include "cons.hh"

//#define ATOM_SMOB

/** a group of individually translated symbols. You can add molecules
    to the top, to the right, etc.

    Dimension behavior:

    Empty molecules have empty dimensions.  If add_at_edge is used to
    init the molecule, we assume that
    DIMENSIONS = (Interval(0,0),Interval(0,0)

*/
class Molecule {
#ifdef ATOM_SMOB
  SCM atom_list_;
#else
  //  Protected_scm atom_list_;	// change to List<Atom>?
  Cons<Atom> *atom_list_;
#endif
  friend class Paper_outputter;

public:
  Box dim_;

  Molecule();
  ~Molecule();

  /**
     Set dimensions to empty, or to (Interval(0,0),Interval(0,0) */
  void set_empty (bool);
  
  void add_at_edge (Axis a, Direction d, const Molecule &m, Real padding);
  void add_atom (Atom const *a);    
  void add_molecule (Molecule const &m);
  void translate (Offset);
  
  /**
     align D direction in axis A.

     If D == CENTER, then move the dimension(A).center() to (0,0)

     Else, move so dimension(A)[D] == 0.0
     
   */
  void align_to (Axis a, Direction d);
  void translate_axis (Real,Axis);

  
  /// how big is #this#? 
  Box extent() const;
  Interval extent (Axis) const;

  Molecule (const Molecule&s);
  void print() const;
  void operator=(const Molecule&);  
  bool empty_b() const;
};
#endif
