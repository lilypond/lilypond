/*
  axis-group-interface.cc -- implement Axis_group_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "axis-group-interface.hh"

#include "align-interface.hh"
#include "pointer-group-interface.hh"
#include "grob.hh"
#include "grob-array.hh"
#include "hara-kiri-group-spanner.hh"
#include "international.hh"
#include "item.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "system.hh"
#include "warn.hh"

void
Axis_group_interface::add_element (Grob *me, Grob *e)
{
  SCM axes = me->get_property ("axes");
  if (!scm_is_pair (axes))
    programming_error ("axes should be nonempty");

  for (SCM ax = axes; scm_is_pair (ax); ax = scm_cdr (ax))
    {
      Axis a = (Axis) scm_to_int (scm_car (ax));

      if (!e->get_parent (a))
	e->set_parent (me, a);

      e->set_object ((a == X_AXIS)
		     ? ly_symbol2scm ("axis-group-parent-X")
		     : ly_symbol2scm ("axis-group-parent-Y"),
		     me->self_scm ());
    }

  /* must be ordered, because Align_interface also uses
     Axis_group_interface  */
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("elements"), e);
}

bool
Axis_group_interface::has_axis (Grob *me, Axis a)
{
  SCM axes = me->get_property ("axes");

  return (SCM_BOOL_F != scm_memq (scm_from_int (a), axes));
}

Interval
Axis_group_interface::relative_group_extent (vector<Grob*> const &elts,
					     Grob *common, Axis a)
{
  Interval r;
  for (vsize i = 0; i < elts.size (); i++)
    {
      Grob *se = elts[i];
      Interval dims = se->extent (common, a);
      if (!dims.is_empty ())
	r.unite (dims);
    }
  return r;
}

Interval
Axis_group_interface::cached_pure_height (Grob *me,
					  vector<Grob*> const &elts,
					  Grob *common,
					  int start, int end)
{
  Paper_score *ps = get_root_system (me)->paper_score ();
  vector<vsize> breaks = ps->get_break_indices ();
  vector<Grob*> cols = ps->get_columns ();
  vsize start_index = VPOS;
  vsize end_index = VPOS;

  for (vsize i = 0; i < breaks.size (); i++)
    {
      int r = Paper_column::get_rank (cols[breaks[i]]);
      if (start == r)
	start_index = i;
      if (end == r)
	end_index = i;
    }

  if (start_index == VPOS || end_index == VPOS)
    {
      programming_error (_ ("tried to calculate pure-height at a non-breakpoint"));
      return Interval (0, 0);
    }

  SCM extents = me->get_property ("cached-pure-extents");
  if (!scm_is_vector (extents))
    {
      extents = scm_c_make_vector (breaks.size () - 1, SCM_EOL);
      for (vsize i = 0; i + 1 < breaks.size (); i++)
	{
	  int st = Paper_column::get_rank (cols[breaks[i]]);
	  int ed = Paper_column::get_rank (cols[breaks[i+1]]);
	  Interval iv = relative_pure_height (me, elts, common, st, ed, false);
	  scm_vector_set_x (extents, scm_from_int (i), ly_interval2scm (iv));
	}
      me->set_property ("cached-pure-extents", extents);
    }

  Interval ext (0, 0);
  for (vsize i = start_index; i < end_index; i++)
    ext.unite (ly_scm2interval (scm_c_vector_ref (extents, i)));
  return ext;
}

Interval
Axis_group_interface::relative_pure_height (Grob *me,
					    vector<Grob*> const &elts,
					    Grob *common,
					    int start, int end,
					    bool use_cache)
{
  /* It saves a _lot_ of time if we assume a VerticalAxisGroup is additive
     (ie. height (i, k) = height (i, j) + height (j, k) for all i <= j <= k).
     Unfortunately, it isn't always true, particularly if there is a
     VerticalAlignment somewhere in the descendants.

     Apart from PianoStaff, which has a fixed VerticalAlignment so it doesn't
     count, the only VerticalAlignment comes from Score. This makes it
     reasonably safe to assume that if our parent is a VerticalAlignment,
     we can assume additivity and cache things nicely. */
  Grob *p = me->get_parent (Y_AXIS);
  if (use_cache && p && Align_interface::has_interface (p))
    return Axis_group_interface::cached_pure_height (me, elts, common, start, end);

  Interval r;

  for (vsize i = 0; i < elts.size (); i++)
    {
      Interval_t<int> rank_span = elts[i]->spanned_rank_iv ();
      Item *it = dynamic_cast<Item*> (elts[i]);
      if (rank_span[LEFT] <= end && rank_span[RIGHT] >= start && (!it || it->pure_is_visible (start, end)))
	{
	  Interval dims = elts[i]->pure_height (common, start, end);
	  if (!dims.is_empty ())
	    r.unite (dims);
	}
    }
  return r;
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, width, 1);
SCM
Axis_group_interface::width (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return generic_group_extent (me, X_AXIS);
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, height, 1);
SCM
Axis_group_interface::height (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return generic_group_extent (me, Y_AXIS);
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, pure_height, 3);
SCM
Axis_group_interface::pure_height (SCM smob, SCM start_scm, SCM end_scm)
{
  int start = robust_scm2int (start_scm, 0);
  int end = robust_scm2int (end_scm, INT_MAX);
  Grob *me = unsmob_grob (smob);

  /* Maybe we are in the second pass of a two-pass spacing run. In that
     case, the Y-extent of a system is already given to us */
  System *system = dynamic_cast<System*> (me);
  if (system)
    {
      SCM line_break_details = system->column (start)->get_property ("line-break-system-details");
      SCM system_y_extent = scm_assq (ly_symbol2scm ("system-Y-extent"), line_break_details);
      if (scm_is_pair (system_y_extent))
	return scm_cdr (system_y_extent);
    }

  return pure_group_height (me, start, end);
}
  
SCM
Axis_group_interface::generic_group_extent (Grob *me, Axis a)
{
  extract_grob_set (me, "elements", elts);
  Grob *common = common_refpoint_of_array (elts, me, a);

  Real my_coord = me->relative_coordinate (common, a);
  Interval r (relative_group_extent (elts, common, a));

  return ly_interval2scm (r - my_coord);
}

SCM
Axis_group_interface::pure_group_height (Grob *me, int start, int end)
{
  Grob *common = unsmob_grob (me->get_object ("common-refpoint-of-elements"));

  if (!common)
    {
      extract_grob_set (me, "elements", elts);

      vector<Grob*> relevant_elts;
      SCM pure_relevant_p = ly_lily_module_constant ("pure-relevant?");

      for (vsize i = 0; i < elts.size (); i++)
	{
	  if (to_boolean (scm_apply_1 (pure_relevant_p, elts[i]->self_scm (), SCM_EOL)))
	    relevant_elts.push_back (elts[i]);

	  Item *it = dynamic_cast<Item*> (elts[i]);
	  Direction d = LEFT;
	  if (it)
	    do
	      {
		Item *piece = it->find_prebroken_piece (d);
		if (piece && to_boolean (scm_apply_1 (pure_relevant_p, piece->self_scm (), SCM_EOL)))
		  relevant_elts.push_back (piece);
	      }
	    while (flip (&d) != LEFT);
	}

      common = common_refpoint_of_array (relevant_elts, me, Y_AXIS);
      me->set_object ("common-refpoint-of-elements", common->self_scm ());

      SCM ga_scm = Grob_array::make_array ();
      Grob_array *ga = unsmob_grob_array (ga_scm);
      ga->set_array (relevant_elts);
      me->set_object ("pure-relevant-elements", ga_scm);
    }

  extract_grob_set (me, "pure-relevant-elements", elts);
  Real my_coord = me->relative_coordinate (common, Y_AXIS);
  Interval r (relative_pure_height (me, elts, common, start, end, true));

  return ly_interval2scm (r - my_coord);
}

void
Axis_group_interface::get_children (Grob *me, vector<Grob*> *found)
{
  found->push_back (me);

  if (!has_interface (me))
    return;

  extract_grob_set (me, "elements", elements);
  for (vsize i = 0; i < elements.size (); i++)
    {
      Grob *e = elements[i];
      Axis_group_interface::get_children (e, found);
    }
}

ADD_INTERFACE (Axis_group_interface, "axis-group-interface",

	       "An object that groups other layout objects.",

	       /* properties */
	       "axes "
	       "elements "
	       "common-refpoint-of-elements "
	       "pure-relevant-elements "
	       "cached-pure-extents "
	       );
