/*
  grob-interface-scheme.cc -- implement grob interface bindings.

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "lily-guile.hh"
#include "protected-scm.hh"
#include "string.hh"

Protected_scm all_ifaces;

LY_DEFINE (ly_add_interface, "ly:add-interface", 3, 0, 0, (SCM a, SCM b, SCM c),
	  "Add an interface description.")
{
  SCM_ASSERT_TYPE (scm_is_symbol (a), a, SCM_ARG1, __FUNCTION__, "symbol");
  SCM_ASSERT_TYPE (scm_is_string (b), b, SCM_ARG2, __FUNCTION__, "string");  
  SCM_ASSERT_TYPE (ly_c_list_p (c), c, SCM_ARG3, __FUNCTION__, "list of syms");
  if (!scm_is_vector (all_ifaces))
    all_ifaces = scm_make_vector (scm_int2num (40), SCM_EOL);

  SCM entry = scm_list_n (a, b, c, SCM_UNDEFINED);

  scm_hashq_set_x (all_ifaces, a, entry);

  return SCM_UNSPECIFIED;
}


LY_DEFINE (ly_all_grob_interfaces, "ly:all-grob-interfaces",
	  0, 0, 0, (),
	  "Get a hash table with all interface descriptions.")
{
  return all_ifaces;
}

