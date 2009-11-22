/*
  tuplet-bracket.hh -- part of GNU LilyPond

  (c) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef Tuplet_bracket_HH
#define Tuplet_bracket_HH

#include "lily-proto.hh"
#include "std-vector.hh"
#include "grob-interface.hh"

class Tuplet_bracket
{
public:
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_positions, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_control_points, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_connect_to_neighbors, (SCM smob));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
  
  DECLARE_GROB_INTERFACE();
  static Grob* get_common_x (Spanner *);
  static void add_tuplet_bracket (Grob *me, Grob *smaller_bracket);
  static void get_bounds (Grob *, Grob **, Grob **);
  static void add_column (Grob *me, Item *);
  static void add_beam (Grob *me, Grob *);
  static Grob *parallel_beam (Grob *me, vector<Grob*> const &cols,
			      bool *equally_long);
  static void calc_position_and_height (Grob *, Real *, Real *dy);
  static Stencil make_bracket (Grob *me, Axis protrusion_axis,
			       Offset dz, Drul_array<Real> height,
			       Interval gap, Drul_array<Real> widen,
			       Drul_array<Real> shorten);
  static Direction get_default_dir (Grob *);
};

#endif // Tuplet_bracket_HH

