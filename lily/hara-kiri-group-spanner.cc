/*
  hara-kiri-vertical-group-spanner.cc -- implement Hara_kiri_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "axis-group-interface.hh"
#include "spanner.hh"
#include "hara-kiri-group-spanner.hh"
#include "debug.hh"
#include "item.hh"


MAKE_SCHEME_CALLBACK (Hara_kiri_group_spanner,y_extent,2);
SCM
Hara_kiri_group_spanner::y_extent (SCM element_smob, SCM scm_axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (scm_axis);

  assert (a == Y_AXIS);
  consider_suicide (me);
  return  Axis_group_interface::group_extent_callback (me->self_scm (), scm_axis);
}


void
Hara_kiri_group_spanner::consider_suicide (Grob*me)
{
  SCM worth = me->get_grob_property ("items-worth-living");
  if (gh_pair_p (worth))
    return ;

  Link_array<Grob> childs = Axis_group_interface::get_children (me);
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
MAKE_SCHEME_CALLBACK (Hara_kiri_group_spanner,force_hara_kiri_callback,2);
SCM
Hara_kiri_group_spanner::force_hara_kiri_callback (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  assert (a == Y_AXIS);
  consider_suicide (me);
  return gh_double2scm (0.0);
}


MAKE_SCHEME_CALLBACK (Hara_kiri_group_spanner,force_hara_kiri_in_parent_callback,2);
SCM
Hara_kiri_group_spanner::force_hara_kiri_in_parent_callback (SCM element_smob, SCM axis)
{
  Grob *daughter = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  assert (a == Y_AXIS);
  force_hara_kiri_callback (daughter->get_parent (a)->self_scm (), axis);
  return gh_double2scm (0.0);
}

void
Hara_kiri_group_spanner::add_element (Grob * me, Grob *e)
{
  //  e->add_offset_callback (force_hara_kiri_in_parent_callback, Y_AXIS);
  Axis_group_interface::add_element (me, e);
}


void
Hara_kiri_group_spanner::set_interface (Grob*me)
{
  me->set_interface (ly_symbol2scm ("hara-kiri-group-interface"));
}


bool
Hara_kiri_group_spanner::has_interface (Grob*me)
{
  return me->has_interface (ly_symbol2scm ("hara-kiri-group-interface"));
}

void 
Hara_kiri_group_spanner::add_interesting_item (Grob* me,Grob* n)
{
  me->add_dependency (n);
  Pointer_group_interface::add_element (me, ly_symbol2scm ("items-worth-living"),n);
}
