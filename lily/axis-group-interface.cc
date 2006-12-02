/*
  axis-group-interface.cc -- implement Axis_group_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "axis-group-interface.hh"

#include "align-interface.hh"
#include "directional-element-interface.hh"
#include "pointer-group-interface.hh"
#include "grob-array.hh"
#include "hara-kiri-group-spanner.hh"
#include "international.hh"
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
      for (vsize i = 0; i < breaks.size () - 1; i++)
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

  return pure_group_height (me, start, end);
}
  
SCM
Axis_group_interface::generic_group_extent (Grob *me, Axis a)
{
  extract_grob_set (me, "elements", elts);
  if (a == Y_AXIS && to_boolean (me->get_property ("skyline-spacing")))
    skyline_spacing (me, elts);
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
      SCM is_relevant = ly_lily_module_constant ("pure-relevant");

      for (vsize i = 0; i < elts.size (); i++)
	{
	  if (to_boolean (scm_apply_1 (is_relevant, elts[i]->self_scm (), SCM_EOL)))
	    relevant_elts.push_back (elts[i]);

	  Item *it = dynamic_cast<Item*> (elts[i]);
	  Direction d = LEFT;
	  if (it)
	    do
	      {
		Item *piece = it->find_prebroken_piece (d);
		if (piece && to_boolean (scm_apply_1 (is_relevant, piece->self_scm (), SCM_EOL)))
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

bool
staff_priority_less (Grob * const &g1, Grob * const &g2)
{
  int priority_1 = robust_scm2int (g1->get_property ("outside-staff-priority"), INT_MIN);
  int priority_2 = robust_scm2int (g2->get_property ("outside-staff-priority"), INT_MIN);

  if (priority_1 < priority_2)
    return true;
  else if (priority_1 > priority_2)
    return false;

  /* if there is no preference in staff priority, choose the one with the lower rank */
  int rank_1 = g1->spanned_rank_iv ()[LEFT];
  int rank_2 = g2->spanned_rank_iv ()[LEFT];
  return rank_1 < rank_2;
}

static void
add_boxes (Grob *me, Grob *x_common, Grob *y_common, vector<Box> *const boxes)
{
  if (Axis_group_interface::has_interface (me)
      && Axis_group_interface::has_axis (me, Y_AXIS))
    {
      Grob_array *elements = unsmob_grob_array (me->get_object ("elements"));
      if (elements)
	for (vsize i = 0; i < elements->size (); i++)
	  add_boxes (elements->grob (i), x_common, y_common, boxes);
    }
  else
    boxes->push_back (Box (me->extent (x_common, X_AXIS),
			   me->extent (y_common, Y_AXIS)));
}

void
Axis_group_interface::skyline_spacing (Grob *me, vector<Grob*> elements)
{
  vector_sort (elements, staff_priority_less);
  Grob *x_common = common_refpoint_of_array (elements, me, X_AXIS);
  Grob *y_common = common_refpoint_of_array (elements, me, Y_AXIS);

  vsize i = 0;
  vector<Box> boxes;

  for (i = 0; i < elements.size ()
  	 && !scm_is_number (elements[i]->get_property ("outside-staff-priority")); i++)
    add_boxes (elements[i], x_common, y_common, &boxes);

  Drul_array<Skyline> skylines (Skyline (boxes, X_AXIS, DOWN),
				Skyline (boxes, X_AXIS, UP));
  for (; i < elements.size (); i++)
    {
      Direction dir = get_grob_direction (elements[i]);
      if (dir == CENTER)
	{
	  warning (_ ("an outside-staff object should have a direction"));
	  continue;
	}

      Box b (elements[i]->extent (x_common, X_AXIS),
	     elements[i]->extent (y_common, Y_AXIS));
      if (b[X_AXIS].is_empty () || b[Y_AXIS].is_empty ())
	{
	  warning (_f ("outside-staff object %s has an empty extent", elements[i]->name ().c_str ()));
	  continue;
	}

      boxes.clear ();
      boxes.push_back (b);
      Skyline other = Skyline (boxes, X_AXIS, -dir);
      Real padding = robust_scm2double (elements[i]->get_property ("outside-staff-padding"), 0.5);
      Real dist = skylines[dir].distance (other) + padding;

      if (dist > 0)
	{
	  b.translate (Offset (0, dir*dist));
	  elements[i]->translate_axis (dir*dist, Y_AXIS);
	}
      skylines[dir].insert (b, X_AXIS);
    }
}

ADD_INTERFACE (Axis_group_interface,

	       "An object that groups other layout objects.",

	       /* properties */
	       "axes "
	       "elements "
	       "common-refpoint-of-elements "
	       "pure-relevant-elements "
	       "skyline-spacing "
	       "cached-pure-extents "
	       );
