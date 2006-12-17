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
#include "separation-item.hh"
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
  if (end == INT_MAX)
    end_index = breaks.size () - 1;

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

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_skylines, 1);
SCM
Axis_group_interface::calc_skylines (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "elements", elts);
  return skyline_spacing (me, elts).smobbed_copy ();
}

/* whereas calc_skylines calculates skylines for axis-groups with a lot of
   visible children, combine_skylines is designed for axis-groups whose only
   children are other axis-groups (ie. VerticalAlignment). Rather than
   calculating all the skylines from scratch, we just merge the skylines
   of the children.
*/
MAKE_SCHEME_CALLBACK (Axis_group_interface, combine_skylines, 1);
SCM
Axis_group_interface::combine_skylines (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "elements", elements);
  Grob *y_common = common_refpoint_of_array (elements, me, Y_AXIS);

  assert (y_common == me);

  Skyline_pair ret;
  for (vsize i = 0; i < elements.size (); i++)
    {
      SCM skyline_scm = elements[i]->get_property ("skylines");
      if (Skyline_pair::unsmob (skyline_scm))
	{
	  Real offset = elements[i]->relative_coordinate (y_common, Y_AXIS);
	  Skyline_pair other = *Skyline_pair::unsmob (skyline_scm);
	  other.raise (offset);
	  ret.merge (other);
	}
    }
  return ret.smobbed_copy ();
}
  
SCM
Axis_group_interface::generic_group_extent (Grob *me, Axis a)
{
  /* trigger the callback to do skyline-spacing on the children */
  (void) me->get_property ("skylines");

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
  Real priority_1 = robust_scm2double (g1->get_property ("outside-staff-priority"), -infinity_f);
  Real priority_2 = robust_scm2double (g2->get_property ("outside-staff-priority"), -infinity_f);

  if (priority_1 < priority_2)
    return true;
  else if (priority_1 > priority_2)
    return false;

  /* if there is no preference in staff priority, choose the left-most one */
  Grob *common = g1->common_refpoint (g2, X_AXIS);
  Real start_1 = g1->extent (common, X_AXIS)[LEFT];
  Real start_2 = g2->extent (common, X_AXIS)[LEFT];
  return start_1 < start_2;
}

static void
add_boxes (Grob *me, Grob *x_common, Grob *y_common, vector<Box> *const boxes)
{
  /* if we are a parent, consider the children's boxes instead of mine */
  if (Grob_array *elements = unsmob_grob_array (me->get_object ("elements")))
    {
      for (vsize i = 0; i < elements->size (); i++)
	add_boxes (elements->grob (i), x_common, y_common, boxes);
    }
  else if (!scm_is_number (me->get_property ("outside-staff-priority")))
    boxes->push_back (Box (me->extent (x_common, X_AXIS),
			   me->extent (y_common, Y_AXIS)));
}

/* We want to avoid situations like this:
           still more text
      more text
   text
   -------------------
   staff
   -------------------

   The point is that "still more text" should be positioned under
   "more text".  In order to achieve this, we place the grobs in several
   passes.  We keep track of the right-most horizontal position that has been
   affected by the current pass so far (actually we keep track of 2
   positions, one for above the staff, one for below).

   In each pass, we loop through the unplaced grobs from left to right.
   If the grob overlaps the right-most affected position, we place it
   (and then update the right-most affected position to point to the right
   edge of the just-placed grob).  Otherwise, we skip it until the next pass.
*/
static void
add_grobs_of_one_priority (Skyline_pair *const skylines,
			   vector<Grob*> elements,
			   Grob *x_common,
			   Grob *y_common)
{
  vector<Box> boxes;
  Drul_array<Real> last_affected_position;

  reverse (elements);
  while (!elements.empty ())
    {
      last_affected_position[UP] = -infinity_f;
      last_affected_position[DOWN] = -infinity_f;
      /* do one pass */
      for (vsize i = elements.size (); i--;)
	{
	  Direction dir = get_grob_direction (elements[i]);
	  if (dir == CENTER)
	    {
	      warning (_ ("an outside-staff object should have a direction, defaulting to up"));
	      dir = UP;
	    }

	  Box b (elements[i]->extent (x_common, X_AXIS),
		 elements[i]->extent (y_common, Y_AXIS));
	  SCM horizon_padding_scm = elements[i]->get_property ("outside-staff-horizontal-padding");
	  Real horizon_padding = robust_scm2double (horizon_padding_scm, 0.0);

	  if (b[X_AXIS][LEFT] - 2*horizon_padding < last_affected_position[dir])
	    continue;

	  if (b[X_AXIS].is_empty () || b[Y_AXIS].is_empty ())
	    warning (_f ("outside-staff object %s has an empty extent", elements[i]->name ().c_str ()));
	  else
	    {
	      boxes.clear ();
	      boxes.push_back (b);
	      Skyline other = Skyline (boxes, horizon_padding, X_AXIS, -dir);
	      Real padding = robust_scm2double (elements[i]->get_property ("outside-staff-padding"), 0.5);
	      Real dist = (*skylines)[dir].distance (other) + padding;

	      if (dist > 0)
		{
		  b.translate (Offset (0, dir*dist));
		  elements[i]->translate_axis (dir*dist, Y_AXIS);
		}
	      (*skylines)[dir].insert (b, 0, X_AXIS);
	      elements[i]->set_property ("outside-staff-priority", SCM_BOOL_F);
	      last_affected_position[dir] = b[X_AXIS][RIGHT];
	    }
	  elements.erase (elements.begin () + i);
	}
    }
}

Skyline_pair
Axis_group_interface::skyline_spacing (Grob *me, vector<Grob*> elements)
{
  vector_sort (elements, staff_priority_less);
  Grob *x_common = common_refpoint_of_array (elements, me, X_AXIS);
  Grob *y_common = common_refpoint_of_array (elements, me, Y_AXIS);

  assert (y_common == me);

  vsize i = 0;
  vector<Box> boxes;

  for (i = 0; i < elements.size ()
  	 && !scm_is_number (elements[i]->get_property ("outside-staff-priority")); i++)
    add_boxes (elements[i], x_common, y_common, &boxes);

  Skyline_pair skylines (boxes, 0, X_AXIS);
  for (; i < elements.size (); i++)
    {
      SCM priority = elements[i]->get_property ("outside-staff-priority");
      vector<Grob*> current_elts;
      current_elts.push_back (elements[i]);
      while (i < elements.size () - 1
	     && scm_eq_p (elements[i+1]->get_property ("outside-staff-priority"), priority))
	current_elts.push_back (elements[++i]);

      add_grobs_of_one_priority (&skylines, current_elts, x_common, y_common);
    }
  return skylines;
}

ADD_INTERFACE (Axis_group_interface,

	       "An object that groups other layout objects.",

	       /* properties */
	       "axes "
	       "elements "
	       "common-refpoint-of-elements "
	       "pure-relevant-elements "
	       "skylines "
	       "cached-pure-extents "
	       );
