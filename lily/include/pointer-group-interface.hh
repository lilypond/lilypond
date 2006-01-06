/*
  pointer-group-interface.hh -- declare Pointer_group_interface

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef POINTER_GROUP_INTERFACE_HH
#define POINTER_GROUP_INTERFACE_HH

#include "parray.hh"
#include "lily-proto.hh"
#include "lily-guile.hh"

struct Pointer_group_interface
{
public:
  static int count (Grob *, SCM);
  static void add_grob (Grob *, SCM nm, Grob *e);
  static void add_grob (Grob *, SCM nm, SCM x);
};

Link_array<Grob> const &internal_extract_grob_array (Grob const *elt, SCM symbol);
Link_array<Item> internal_extract_item_array (Grob const *elt, SCM symbol);

#define extract_grob_array(x, prop) internal_extract_grob_array (x, ly_symbol2scm (prop))
#define extract_item_array(x, prop) internal_extract_item_array (x, ly_symbol2scm (prop))

/*
  This is dubious coding style, but lets not risk that we change the
  representation of grob sets again.
*/
#define extract_grob_set(grob, prop, set)				\
  Link_array<Grob> const &set (internal_extract_grob_array (grob, ly_symbol2scm (prop)))
#define extract_item_set(grob, prop, set)				\
  Link_array<Item> set (internal_extract_item_array (grob, ly_symbol2scm (prop)))

#endif /* POINTER_GROUP_INTERFACE_HH */

