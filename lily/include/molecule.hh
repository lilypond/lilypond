/*
  molecule.hh -- declare Molecule

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#ifndef MOLECULE_HH
#define MOLECULE_HH

#include "lily-proto.hh"
#include "box.hh"
#include "axes.hh"
#include "direction.hh"
#include "cons.hh"
#include "protected-scm.hh"

//#define ATOM_SMOB

/** a group of individually translated symbols. You can add molecules
    to the top, to the right, etc.

    Dimension behavior:

    Empty molecules have empty dimensions.  If add_at_edge is used to
    init the molecule, we assume that
    DIMENSIONS = (Interval(0,0),Interval(0,0)

*/
class Molecule {
  Protected_scm atom_list_;

  friend class Paper_outputter;

public:
  Box dim_;

  Molecule();
  ~Molecule();

  /**
     Set dimensions to empty, or to (Interval(0,0),Interval(0,0) */
  void set_empty (bool);
  void add_at_edge (Axis a, Direction d, const Molecule &m, Real padding);

  /**
     Add an atom.  The molecule assumes responsibility for cleaning.
   */
  void add_atom (SCM as);    
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

  void operator=(const Molecule&);  
  bool empty_b() const;
  void print ()const;
};
#endif
