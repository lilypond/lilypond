/*
  atom.hh -- declare Atom

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef ATOM_HH
#define ATOM_HH

#include "lily-proto.hh"
#include "box.hh"
#include "lily-guile.hh"

SCM translate_atom (Offset, SCM);
SCM translate_atom_axis (Real, Axis,SCM); 
SCM fontify_atom (Font_metric*, SCM atom);

#endif
