/*
  tuplet-bracket.hh -- part of GNU LilyPond

  (c) 1997--2005 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef Tuplet_bracket_HH
#define Tuplet_bracket_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

class Tuplet_bracket
{
public:
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));

  static bool has_interface (Grob *);
  static void add_column (Grob *me, Item *);
  static void add_beam (Grob *me, Grob *);
  static Grob *parallel_beam (Grob *me, Link_array<Grob> const &cols, bool *equally_long);
  static void calc_position_and_height (Grob *, Real *, Real *dy);
  static Stencil make_bracket (Grob *me, Axis protusion_axis,
			       Offset dz, Drul_array<Real> height,
			       Interval gap, Drul_array<Real> widen,
			       Drul_array<Real> shorten);
  static Direction get_default_dir (Grob *);
};

#endif // Tuplet_bracket_HH

