/*
  hara-kiri-vertical-group-spanner.cc 
    -- implement Hara_kiri_vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "axis-group-interface.hh"
#include "hara-kiri-vertical-group-spanner.hh"
#include "debug.hh"
#include "item.hh"

Hara_kiri_group_spanner::Hara_kiri_group_spanner(SCM s)
  : Spanner (s)
{
  set_elt_pointer ("items-worth-living", SCM_EOL);
}

void 
Hara_kiri_group_spanner::add_interesting_item (Item* n)
{
  add_dependency (n);
  Pointer_group_interface (this, "items-worth-living").add_element (n);
}

void 
Hara_kiri_group_spanner::after_line_breaking ()
{
  SCM worth = get_elt_pointer ("items-worth-living");
  if (gh_pair_p (worth))
    return;

  Link_array<Score_element> childs = Axis_group_interface (this).get_children ();
  for (int i = 0; i < childs.size (); i++)
    {
      Score_element* s = childs[i];

      if ( line_l () != s->line_l ())
	programming_error ("Killing other children too");
      s->suicide ();
    }
  set_extent_callback (0, X_AXIS);
  set_extent_callback (0, Y_AXIS);  
}



