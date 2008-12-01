/*
  grob-interface-scheme.cc -- implement grob interface bindings.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "lily-guile.hh"
#include "std-string.hh"

static SCM all_ifaces;

void
internal_add_interface (SCM a, SCM b, SCM c)
{
  if (!all_ifaces)
    {
      SCM tab = scm_c_make_hash_table (59);
      all_ifaces = tab;
      scm_permanent_object (tab);
    }
  
  SCM entry = scm_list_n (a, b, c, SCM_UNDEFINED);

  scm_hashq_set_x (all_ifaces, a, entry);
}

LY_DEFINE (ly_add_interface, "ly:add-interface",
	   3, 0, 0, (SCM a, SCM b, SCM c),
	   "Add an interface description.")
{
  LY_ASSERT_TYPE (ly_is_symbol, a, 1);
  LY_ASSERT_TYPE (scm_is_string, b, 2);
  LY_ASSERT_TYPE (ly_is_list, c, 3);

  internal_add_interface (a,b,c);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_all_grob_interfaces, "ly:all-grob-interfaces",
	   0, 0, 0, (),
	   "Get a hash table with all interface descriptions.")
{
  return all_ifaces;
}

