/*
  hara-kiri-vertical-group-spanner.cc -- implement Hara_kiri_group_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1998--2006 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "hara-kiri-group-spanner.hh"

#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "axis-group-interface.hh"
#include "spanner.hh"
#include "warn.hh"
#include "item.hh"

MAKE_SCHEME_CALLBACK (Hara_kiri_group_spanner, y_extent, 1);
SCM
Hara_kiri_group_spanner::y_extent (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  consider_suicide (me);
  return Axis_group_interface::generic_group_extent (me, Y_AXIS);
}

void
Hara_kiri_group_spanner::consider_suicide (Grob *me)
{
  Spanner *sp = dynamic_cast<Spanner *> (me);
  if (!to_boolean (me->get_property ("remove-empty")))
    return ;

  extract_grob_set (me, "items-worth-living", worth);
  if (worth.size ())
    return;

  bool remove_first = to_boolean (me->get_property ("remove-first"));
  if (!remove_first
       && ((sp->original () && broken_spanner_index (sp) == 0)
	   || Paper_column::get_rank (sp->get_bound (LEFT)->get_column ())
	   == 0)) 
    return;

  Link_array__Grob_ childs;
  Axis_group_interface::get_children (me, &childs);
  for (vsize i = 0; i < childs.size (); i++)
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
MAKE_SCHEME_CALLBACK (Hara_kiri_group_spanner, after_line_breaking, 1);
SCM
Hara_kiri_group_spanner::after_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  consider_suicide (me);
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Hara_kiri_group_spanner, force_hara_kiri_callback, 1);
SCM
Hara_kiri_group_spanner::force_hara_kiri_callback (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  consider_suicide (me);
  return scm_from_double (0.0);
}

MAKE_SCHEME_CALLBACK (Hara_kiri_group_spanner, force_hara_kiri_in_y_parent_callback, 1);
SCM
Hara_kiri_group_spanner::force_hara_kiri_in_y_parent_callback (SCM smob)
{
  Grob *daughter = unsmob_grob (smob);
  force_hara_kiri_callback (daughter->get_parent (Y_AXIS)->self_scm ());
  return scm_from_double (0.0);
}

void
Hara_kiri_group_spanner::add_interesting_item (Grob *me, Grob *n)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("items-worth-living"), n);
}

ADD_INTERFACE (Hara_kiri_group_spanner, "hara-kiri-group-interface",
	       "A group spanner that  keeps track of interesting items.  If it "
	       "doesn't contain any after linebreaking, then it "
	       "will remove itself and all its children.",


	       /* properties */
	       "items-worth-living "
	       "remove-empty "
	       "remove-first "
	       );


