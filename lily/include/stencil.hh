/*
  stencil.hh -- declare Stencil

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#ifndef STENCIL_HH
#define STENCIL_HH

#include <cstdlib>		// size_t

#include "lily-proto.hh"
#include "box.hh"
#include "smobs.hh"

/** a group of individually translated symbols. You can add stencils
    to the top, to the right, etc.

    It is implemented as a "tree" of scheme expressions, as in

     Expr = combine Expr-list
            | translate Offset Expr
	    | origin (ORIGIN) Expr
	    | no-origin Expr
	    | (SCHEME)
	    ;

    SCHEME is a Scheme expression that --when eval'd-- produces the
    desired output.  

    Notes:
    
    * Because of the way that Stencil is implemented, it is the most
    efficient to add "fresh" stencils to what you're going to build.

    * Do not create Stencil objects on the heap. That includes passing
    around Stencil* which are produced by unsmob_stencil(). Either
    copy Stencil objects, or use SCM references.
    
    * Empty stencils have empty dimensions.  If add_at_edge is used to
    init the stencil, we assume that

      DIMENSIONS = (Interval (0,0),Interval (0,0)
*/
class Stencil
{
  friend SCM ly_stencil_set_extent_x (SCM, SCM, SCM);

  /*
    This provides the reference point of the symbol, for example with
    characters, it is on the base line of the character. Usually,
    ORIGIN is inside DIM_
   */
  Offset origin_;
  Box dim_;
  SCM expr_;
  
  DECLARE_SIMPLE_SMOBS (Stencil,);  
public:
  Stencil (Box, SCM s);
  Stencil ();
  
  Offset origin () const;
  SCM expr () const;

  /**
     Set dimensions to empty, or to (Interval (0,0),Interval (0,0) */
  void set_empty (bool);
  Stencil moved_to_edge (Axis a, Direction d, const Stencil &m, Real padding,
			 Real minimum) const;

  void add_at_edge (Axis a, Direction d, const Stencil &m, Real padding,
		    Real minimum);
  void add_stencil (Stencil const &m);
  void translate (Offset);
  void align_to (Axis a, Real x);
  void translate_axis (Real,Axis);
  
  Interval extent (Axis) const;
  Box extent_box () const;
  bool is_empty () const;

  static SCM ly_get_stencil_extent (SCM mol, SCM axis);
  static SCM ly_set_stencil_extent_x (SCM,SCM,SCM);
  static SCM ly_stencil_combined_at_edge (SCM,SCM,SCM,SCM,SCM);
};


DECLARE_UNSMOB(Stencil,stencil);
SCM fontify_atom (Font_metric const*, SCM atom);

void interpret_stencil_expression (SCM expr,
                        void (*func) (void*, SCM),
                        void *func_arg,
                        Offset o);

Stencil create_stencil (SCM print);
SCM find_expression_fonts (SCM expr);

#endif /* STENCIL_HH */
