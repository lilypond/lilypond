/*
  hara-kiri-vertical-group-spanner.cc -- implement Hara_kiri_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "axis-group-interface.hh"
#include "spanner.hh"
#include "hara-kiri-group-spanner.hh"
#include "debug.hh"
#include "item.hh"

void
Hara_kiri_group_spanner::set_interface (Score_element*me)
{
  me->set_elt_property ("items-worth-living", SCM_EOL);
  me->add_offset_callback (force_hara_kiri_callback, Y_AXIS);
  me->set_interface (ly_symbol2scm ("hara-kiri-spanner-interface"));
}

bool
Hara_kiri_group_spanner::has_interface (Score_element*me)
{
  return me->has_interface (ly_symbol2scm ("hara-kiri-spanner-interface"));
}

void 
Hara_kiri_group_spanner::add_interesting_item (Score_element* me,Score_element* n)
{
  me->add_dependency (n);
  Pointer_group_interface (me, "items-worth-living").add_element (n);
}

void
Hara_kiri_group_spanner::consider_suicide(Score_element*me)
{
  SCM worth = me->get_elt_property ("items-worth-living");
  if (gh_pair_p (worth))
    return ;

  Link_array<Score_element> childs = Axis_group_interface::get_children (me);
  for (int i = 0; i < childs.size (); i++)
    childs[i]->suicide ();


  /*
    very appropriate name here :-)
   */
  me->suicide ();
}



/*
  We can't rely on offsets and dimensions of elements in a hara-kiri
  group. Use a callback to make sure that hara-kiri has been done
  before asking for offsets.  */
Real
Hara_kiri_group_spanner::force_hara_kiri_callback (Score_element *elt, Axis a)
{
  assert (a == Y_AXIS);
  consider_suicide (elt);
  return 0.0;
}

