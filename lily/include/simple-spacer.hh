/*
  simple-spacer.hh -- declare Simple_spacer

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SIMPLE_SPACER_HH
#define SIMPLE_SPACER_HH

#include "parray.hh"
#include "lily-proto.hh"
#include "smobs.hh"

struct Spring_description
{
  Real ideal_;
  Real inverse_hooke_;
  bool is_active_;
  Real block_force_;

  Real length (Real force) const;
  Spring_description ();

  bool is_sane () const;
};

class Simple_spacer
{
public:
  std::vector<Spring_description> springs_;
  Real force_;
  Real indent_;
  Real line_len_;
  Real default_space_;
  int active_count_;

  Simple_spacer ();

  void my_solve_linelen ();
  void my_solve_natural_len ();
  Real active_springs_stiffness () const;
  Real range_stiffness (int, int) const;
  void add_rod (int l, int r, Real dist);
  void add_spring (Real, Real);
  Real range_ideal_len (int l, int r) const;
  Real active_blocking_force ()const;
  Real configuration_length ()const;
  void set_active_states ();
  bool is_active () const;

  DECLARE_SIMPLE_SMOBS (Simple_spacer,);
};

struct Simple_spacer_wrapper
{
  Simple_spacer *spacer_;
  Link_array<Grob> spaced_cols_;
  Link_array<Grob> loose_cols_;

  Simple_spacer_wrapper ();
  void add_columns (Link_array<Grob> const &);
  void solve (Column_x_positions *, bool);
  ~Simple_spacer_wrapper ();
private:
  Simple_spacer_wrapper (Simple_spacer_wrapper const &);
};

#endif /* SIMPLE_SPACER_HH */

