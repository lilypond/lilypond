/*   
  simple-spacer.hh -- declare Simple_spacer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SIMPLE_SPACER_HH
#define SIMPLE_SPACER_HH

#include "parray.hh"
#include "line-spacer.hh"


struct Spring_description
{
  Real ideal_f_;
  Real hooke_f_;
  bool active_b_;

  Real block_force_f_;

  Real length (Real force) const;
  Spring_description ();
  Real energy_f (Real) const;
};

/**
   A simple spacing constraint solver. The approach:

   Stretch the line uniformly until none of the constraints (rods)
   block.  It then is very wide.


      Compress until the next constraint blocks,

      Mark the springs over the constrained part to be non-active.
      
   Repeat with the smaller set of non-active constraints, until all
   constraints blocked, or until the line is as short as desired.

   This is much simpler, and much much faster than full scale
   Constrained QP. On the other hand, a situation like this will not
   be typeset as dense as possible, because

   c4                   c4           c4                  c4
   veryveryverylongsyllable2         veryveryverylongsyllable2
   " "4                 veryveryverylongsyllable2        syllable4


   can be further compressed to


   c4    c4                        c4   c4
   veryveryverylongsyllable2       veryveryverylongsyllable2
   " "4  veryveryverylongsyllable2      syllable4


   Perhaps this is not a bad thing, because the 1st looks better anyway.  */
struct Simple_spacer: public Line_spacer
{
  Array<Spring_description> springs_;
  Real force_f_;

  Simple_spacer ();
  
  virtual void solve (Column_x_positions *) const;
  virtual void lower_bound_solution (Column_x_positions *) const;
  virtual void add_columns (Link_array<Paper_column>);
    
  void my_solve_linelen ();
  void my_solve_natural_len ();
  Real active_springs_stiffness () const;
  Real range_stiffness (int, int) const;
  void add_rod (int l, int r, Real dist);
  Real range_ideal_len (int l, int r)const;
  Real active_blocking_force ()const;
  Real configuration_length ()const;
  void set_active_states ();
  Real energy_f () const;

  bool active_b () const;
};

#endif /* SIMPLE_SPACER_HH */

