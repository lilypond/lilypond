/*
  horizontal-bracket.hh -- declare Horizontal_bracket

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef HORIZONTAL_BRACKET_HH
#define HORIZONTAL_BRACKET_HH

#include "lily-proto.hh"
#include "std-vector.hh"
#include "grob-interface.hh"

struct Horizontal_bracket
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static Stencil make_bracket (Grob *, Real, Axis, Direction);
  static Stencil make_enclosing_bracket (Grob *me, Grob *refpoint,
					 vector<Grob*> grobs,
					 Axis a, Direction dir);
  DECLARE_GROB_INTERFACE();
};

#endif /* HORIZONTAL_BRACKET_HH */
