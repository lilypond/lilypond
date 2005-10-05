/*
  horizontal-bracket.hh -- declare Horizontal_bracket

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef HORIZONTAL_BRACKET_HH
#define HORIZONTAL_BRACKET_HH

struct Horizontal_bracket
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static Stencil make_bracket (Grob *, Grob *, Link_array<Grob>, Axis, Direction);
  static bool has_interface (Grob *);
};

#endif /* HORIZONTAL_BRACKET_HH */
