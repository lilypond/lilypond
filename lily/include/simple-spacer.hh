/*
  simple-spacer.hh -- declare Simple_spacer

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SIMPLE_SPACER_HH
#define SIMPLE_SPACER_HH

#include "std-vector.hh"
#include "lily-proto.hh"
#include "spring.hh"
#include "smobs.hh"

class Simple_spacer
{
public:
  Simple_spacer ();

  void solve (Real line_len, bool ragged);
  void add_rod (int l, int r, Real dist);
  void add_spring (Spring const&);
  Real range_ideal_len (int l, int r) const;
  Real range_stiffness (int l, int r, bool stretch) const;
  Real configuration_length (Real) const;
  vector<Real> spring_positions () const;

  Real force () const;
  Real force_penalty (bool ragged) const;
  bool fits () const;

  DECLARE_SIMPLE_SMOBS (Simple_spacer);

private:
  Real expand_line ();
  Real compress_line ();
  Real rod_force (int l, int r, Real dist);

  vector<Spring> springs_;
  Real line_len_;
  Real force_;
  bool ragged_;
  bool fits_;
};

/* returns a vector of dimensions breaks.size () * breaks.size () */
vector<Real> get_line_forces (vector<Grob*> const &columns,
			      Real line_len,
			      Real indent,
			      bool ragged);

Column_x_positions get_line_configuration (vector<Grob*> const &columns,
					   Real line_len,
					   Real indent,
					   bool ragged);

#endif /* SIMPLE_SPACER_HH */

