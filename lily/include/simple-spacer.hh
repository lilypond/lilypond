/*   
  simple-spacer.hh -- declare Simple_spacer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SIMPLE_SPACER_HH
#define SIMPLE_SPACER_HH

#include "parray.hh"
#include "lily-proto.hh"


struct Spring_description
{
  Real ideal_;
  Real hooke_;
  bool active_b_;

  Real block_force_;

  Real length (Real force) const;
  Spring_description ();

  bool sane_b () const;
};

struct Simple_spacer
{
  Array<Spring_description> springs_;
  Link_array<Grob> spaced_cols_;
  Link_array<Grob> loose_cols_;
  Real force_;
  Real indent_;
  Real line_len_;
  Real default_space_;
  int active_count_;
  bool compression_penalty_b_;
  
  Simple_spacer ();
  
  void solve (Column_x_positions *, bool) const;
  void add_columns (Link_array<Grob>const &);
  void my_solve_linelen ();
  void my_solve_natural_len ();
  Real active_springs_stiffness () const;
  Real range_stiffness (int, int) const;
  void add_rod (int l, int r, Real dist);
  Real range_ideal_len (int l, int r)const;
  Real active_blocking_force ()const;
  Real configuration_length ()const;
  void set_active_states ();
  bool active_b () const;
};

#endif /* SIMPLE_SPACER_HH */

