/*
  molecule.hh -- declare Molecule

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#ifndef MOLECULE_HH
#define MOLECULE_HH

#include <stdlib.h>		// size_t
#include "lily-proto.hh"
#include "box.hh"
#include "axes.hh"
#include "direction.hh"
#include "lily-guile.hh"

/** a group of individually translated symbols. You can add molecules
    to the top, to the right, etc.

    It is implemented as a "tree" of scheme expressions, as in

     Expr = combine Expr Expr
              | translate Offset Expr
	      | SCHEME
	      ;

    SCHEME is a Scheme expression that --when eval'd-- produces the
    desired output.  


    Because of the way that Molecule is implemented, it is the most
    efficient to add "fresh" molecules to what you're going to build.
    
    Dimension behavior:

    Empty molecules have empty dimensions.  If add_at_edge is used to
    init the molecule, we assume that
    DIMENSIONS = (Interval(0,0),Interval(0,0)
    
*/
class Molecule {
  /// can't alloc on heap.
  void * operator new (size_t s); 
public:
  Box dim_;
  SCM expr_;
  
  Molecule (Box, SCM s);
  Molecule();

  /**
     Set dimensions to empty, or to (Interval(0,0),Interval(0,0) */
  void set_empty (bool);
  void add_at_edge (Axis a, Direction d, const Molecule &m, Real padding);
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
  bool empty_b() const;
  void print ()const;
};

SCM fontify_atom (Font_metric*, SCM atom);

#endif
