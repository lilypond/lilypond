/*
  staff-grouper-interface.cc -- implement Staff_grouper_interface

  source file of the GNU LilyPond music typesetter

  (c) 2009 Joe Neeman <joeneeman@gmail.com>
*/

#include "staff-grouper-interface.hh"

#include "pointer-group-interface.hh"

Grob*
Staff_grouper_interface::get_last_grob (Grob *me)
{
  extract_grob_set (me, "elements", elts);
  for (vsize i = elts.size (); i--;)
    if (elts[i]->is_live ())
      return elts[i];

  return 0;
}

ADD_INTERFACE (Staff_grouper_interface,
	       "A grob that collects staves together.",

	       /* properties */
	       "between-staff-spacing "
	       "after-last-staff-spacing "
	       );

