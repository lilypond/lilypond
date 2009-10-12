/*
  axis-group-interface.cc -- implement Axis_group_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "axis-group-interface.hh"

#include "align-interface.hh"
#include "directional-element-interface.hh"
#include "grob-array.hh"
#include "hara-kiri-group-spanner.hh"
#include "international.hh"
#include "lookup.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "pointer-group-interface.hh"
#include "separation-item.hh"
#include "skyline-pair.hh"
#include "staff-grouper-interface.hh"
#include "stencil.hh"
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
      if (!to_boolean (se->get_property ("cross-staff")))
	{
	  Interval dims = se->extent (common, a);
	  if (!dims.is_empty ())
	    r.unite (dims);
	}
    }
  return r;
}

Interval
Axis_group_interface::cached_pure_height (Grob *me, int start, int end)
{
  Interval iv = begin_of_line_pure_height (me, start);
  iv.unite (rest_of_line_pure_height (me, start, end));

  return iv;
}

Interval
Axis_group_interface::rest_of_line_pure_height (Grob *me, int start, int end)
{
  SCM adjacent_pure_heights = me->get_property ("adjacent-pure-heights");

  if (!scm_is_pair (adjacent_pure_heights)
      || !scm_is_vector (scm_cdr (adjacent_pure_heights)))
    return Interval (0, 0);

  return combine_pure_heights (me, scm_cdr (adjacent_pure_heights), start, end);
}

Interval
Axis_group_interface::begin_of_line_pure_height (Grob *me, int start)
{
  SCM adjacent_pure_heights = me->get_property ("adjacent-pure-heights");

  if (!scm_is_pair (adjacent_pure_heights)
      || !scm_is_vector (scm_car (adjacent_pure_heights)))
    return Interval (0, 0);

  return combine_pure_heights (me, scm_car (adjacent_pure_heights), start, start+1);
}

Interval
Axis_group_interface::combine_pure_heights (Grob *me, SCM measure_extents, int start, int end)
{
  Paper_score *ps = get_root_system (me)->paper_score ();
  vector<vsize> breaks = ps->get_break_indices ();
  vector<Grob*> cols = ps->get_columns ();

  Interval ext;
  for (vsize i = 0; i + 1 < breaks.size (); i++)
    {
      int r = Paper_column::get_rank (cols[breaks[i]]);
      if (r >= end)
	break;

      if (r >= start)
	ext.unite (ly_scm2interval (scm_c_vector_ref (measure_extents, i)));
    }

  return ext;
}

// adjacent-pure-heights is a pair of vectors, each of which has one element
// for every measure in the score. The first vector stores, for each measure,
// the combined height of the elements that are present only when the bar
// is at the beginning of a line. The second vector stores, for each measure,
// the combined height of the elements that are present only when the bar
// is not at the beginning of a line.

MAKE_SCHEME_CALLBACK (Axis_group_interface, adjacent_pure_heights, 1)
SCM
Axis_group_interface::adjacent_pure_heights (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Grob *common = calc_pure_elts_and_common (me);
  extract_grob_set (me, "pure-relevant-items", items);
  extract_grob_set (me, "pure-relevant-spanners", spanners);

  Paper_score *ps = get_root_system (me)->paper_score ();
  vector<vsize> breaks = ps->get_break_indices ();
  vector<Grob*> cols = ps->get_columns ();

  SCM begin_line_heights = scm_c_make_vector (breaks.size () - 1, SCM_EOL);
  SCM mid_line_heights = scm_c_make_vector (breaks.size () - 1, SCM_EOL);

  vsize it_index = 0;
  for (vsize i = 0; i + 1 < breaks.size (); i++)
    {
      int start = Paper_column::get_rank (cols[breaks[i]]);
      int end = Paper_column::get_rank (cols[breaks[i+1]]);
      Interval begin_line_iv;
      Interval mid_line_iv;

      for (vsize j = it_index; j < items.size (); j++)
	{
	  Item *it = dynamic_cast<Item*> (items[j]);
	  int rank = it->get_column ()->get_rank ();

	  if (rank <= end && it->pure_is_visible (start, end)
	      && !to_boolean (it->get_property ("cross-staff")))
	    {
	      Interval dims = items[j]->pure_height (common, start, end);
	      Interval &target_iv = start == it->get_column ()->get_rank () ? begin_line_iv : mid_line_iv;

	      if (!dims.is_empty ())
		target_iv.unite (dims);
	    }

	  if (rank < end)
	    it_index++;
	  else if (rank > end)
	    break;
	}

      for (vsize j = 0; j < spanners.size (); j++)
	{
	  Interval_t<int> rank_span = spanners[j]->spanned_rank_interval ();
	  if (rank_span[LEFT] <= end && rank_span[RIGHT] >= start
	      && !to_boolean (spanners[j]->get_property ("cross-staff")))
	    {
	      Interval dims = spanners[j]->pure_height (common, start, end);

	      if (!dims.is_empty ())
		mid_line_iv.unite (dims);
	    }
	}

      scm_vector_set_x (begin_line_heights, scm_from_int (i), ly_interval2scm (begin_line_iv));
      scm_vector_set_x (mid_line_heights, scm_from_int (i), ly_interval2scm (mid_line_iv));
    }
  return scm_cons (begin_line_heights, mid_line_heights);
}

Interval
Axis_group_interface::relative_pure_height (Grob *me, int start, int end)
{
  /* It saves a _lot_ of time if we assume a VerticalAxisGroup is additive
     (ie. height (i, k) = max (height (i, j) height (j, k)) for all i <= j <= k).
     Unfortunately, it isn't always true, particularly if there is a
     VerticalAlignment somewhere in the descendants.

     Apart from PianoStaff, which has a fixed VerticalAlignment so it doesn't
     count, the only VerticalAlignment comes from Score. This makes it
     reasonably safe to assume that if our parent is a VerticalAlignment,
     we can assume additivity and cache things nicely. */
  Grob *p = me->get_parent (Y_AXIS);
  if (p && Align_interface::has_interface (p))
    return Axis_group_interface::cached_pure_height (me, start, end);

  Grob *common = calc_pure_elts_and_common (me);
  extract_grob_set (me, "pure-relevant-items", items);
  extract_grob_set (me, "pure-relevant-spanners", spanners);

  Interval r;

  for (vsize i = 0; i < items.size (); i++)
    {
      Item *it = dynamic_cast<Item*> (items[i]);
      int rank = it->get_column ()->get_rank ();

      if (rank > end)
	break;
      else if (rank >= start && it->pure_is_visible (start, end)
	       && !to_boolean (it->get_property ("cross-staff")))
	{
	  Interval dims = it->pure_height (common, start, end);
	  if (!dims.is_empty ())
	    r.unite (dims);
	}
    }

  for (vsize i = 0; i < spanners.size (); i++)
    {
      Interval_t<int> rank_span = spanners[i]->spanned_rank_interval ();
      if (rank_span[LEFT] <= end && rank_span[RIGHT] >= start
	  && !to_boolean (spanners[i]->get_property ("cross-staff")))
	{
	  Interval dims = spanners[i]->pure_height (common, start, end);
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

  return ly_interval2scm (pure_group_height (me, start, end));
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_skylines, 1);
SCM
Axis_group_interface::calc_skylines (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "elements", elts);
  Skyline_pair skylines = skyline_spacing (me, elts);

  return skylines.smobbed_copy ();
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
  Grob *x_common = common_refpoint_of_array (elements, me, X_AXIS);

  if (y_common != me)
    programming_error ("combining skylines that don't belong to me");

  Skyline_pair ret;
  for (vsize i = 0; i < elements.size (); i++)
    {
      SCM skyline_scm = elements[i]->get_property ("vertical-skylines");
      if (Skyline_pair::unsmob (skyline_scm))
	{
	  Real offset = elements[i]->relative_coordinate (y_common, Y_AXIS);
	  Skyline_pair other = *Skyline_pair::unsmob (skyline_scm);
	  other.raise (offset);
	  other.shift (elements[i]->relative_coordinate (x_common, X_AXIS));
	  ret.merge (other);
	}
    }
  return ret.smobbed_copy ();
}
  
SCM
Axis_group_interface::generic_group_extent (Grob *me, Axis a)
{
  /* trigger the callback to do skyline-spacing on the children */
  if (a == Y_AXIS)
    (void) me->get_property ("vertical-skylines");

  extract_grob_set (me, "elements", elts);
  Grob *common = common_refpoint_of_array (elts, me, a);

  Real my_coord = me->relative_coordinate (common, a);
  Interval r (relative_group_extent (elts, common, a));

  return ly_interval2scm (r - my_coord);
}

/* This is like generic_group_extent, but it only counts the grobs that
   are children of some other axis-group. This is uncached; if it becomes
   commonly used, it may be necessary to cache it somehow. */
Interval
Axis_group_interface::staff_extent (Grob *me, Grob *refp, Axis ext_a, Grob *staff, Axis parent_a)
{
  extract_grob_set (me, "elements", elts);
  vector<Grob*> new_elts;

  for (vsize i = 0; i < elts.size (); i++)
    if (elts[i]->common_refpoint (staff, parent_a) == staff)
      new_elts.push_back (elts[i]);

  return relative_group_extent (new_elts, refp, ext_a);
}


Grob *
Axis_group_interface::calc_pure_elts_and_common (Grob *me)
{
  if (Grob *c = unsmob_grob (me->get_object ("pure-Y-common")))
    return c;
  
  extract_grob_set (me, "elements", elts);

  vector<Grob*> relevant_items;
  vector<Grob*> relevant_spanners;
  SCM pure_relevant_p = ly_lily_module_constant ("pure-relevant?");

  for (vsize i = 0; i < elts.size (); i++)
    {
      if (to_boolean (scm_apply_1 (pure_relevant_p, elts[i]->self_scm (), SCM_EOL)))
	{
	  if (dynamic_cast<Item*> (elts[i]))
	    relevant_items.push_back (elts[i]);
	  else if (dynamic_cast<Spanner*> (elts[i]))
	    relevant_spanners.push_back (elts[i]);
	}
	    

      Item *it = dynamic_cast<Item*> (elts[i]);
      Direction d = LEFT;
      if (it)
	do
	  {
	    Item *piece = it->find_prebroken_piece (d);
	    if (piece && to_boolean (scm_apply_1 (pure_relevant_p, piece->self_scm (), SCM_EOL)))
	      relevant_items.push_back (piece);
	  }
	while (flip (&d) != LEFT);
    }
  vector_sort (relevant_items, Item::less);

  Grob *common = common_refpoint_of_array (relevant_items, me, Y_AXIS);
  common = common_refpoint_of_array (relevant_spanners, common, Y_AXIS);

  me->set_object ("pure-Y-common", common->self_scm ());
  
  SCM items_scm = Grob_array::make_array ();
  SCM spanners_scm = Grob_array::make_array ();

  unsmob_grob_array (items_scm)->set_array (relevant_items);
  unsmob_grob_array (spanners_scm)->set_array (relevant_spanners);
  me->set_object ("pure-relevant-items", items_scm);
  me->set_object ("pure-relevant-spanners", spanners_scm);

  return common;
}

SCM
Axis_group_interface::calc_common (Grob *me, Axis axis)
{
  extract_grob_set (me, "elements", elts);
  Grob *common = common_refpoint_of_array (elts, me, axis);
  if (!common)
    {
      me->programming_error ("No common parent found in calc_common axis.");
      return SCM_EOL;
    }
  
  return common->self_scm ();
}


MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_x_common, 1);
SCM
Axis_group_interface::calc_x_common (SCM grob)
{
  return calc_common (unsmob_grob (grob), X_AXIS);
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_y_common, 1);
SCM
Axis_group_interface::calc_y_common (SCM grob)
{
  return calc_common (unsmob_grob (grob), Y_AXIS);
}

Interval
Axis_group_interface::pure_group_height (Grob *me, int start, int end)
{
  Grob *common = calc_pure_elts_and_common (me);
	
  Real my_coord = me->relative_coordinate (common, Y_AXIS);
  Interval r (relative_pure_height (me, start, end));

  return r - my_coord;
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

  /* if neither grob has an outside-staff priority, the ordering will have no
     effect -- we just need to choose a consistent ordering. We do this to
     avoid the side-effect of calculating extents. */
  if (isinf (priority_1))
    return g1 < g2;

  /* if there is no preference in staff priority, choose the left-most one */
  Grob *common = g1->common_refpoint (g2, X_AXIS);
  Real start_1 = g1->extent (common, X_AXIS)[LEFT];
  Real start_2 = g2->extent (common, X_AXIS)[LEFT];
  return start_1 < start_2;
}

static void
add_boxes (Grob *me, Grob *x_common, Grob *y_common, vector<Box> *const boxes, Skyline_pair *skylines)
{
  /* if a child has skylines, use them instead of the extent box */
  if (Skyline_pair *pair = Skyline_pair::unsmob (me->get_property ("vertical-skylines")))
    {
      Skyline_pair s = *pair;
      s.shift (me->relative_coordinate (x_common, X_AXIS));
      s.raise (me->relative_coordinate (y_common, Y_AXIS));
      skylines->merge (s);
    }
  else if (Grob_array *elements = unsmob_grob_array (me->get_object ("elements")))
    {
      for (vsize i = 0; i < elements->size (); i++)
	add_boxes (elements->grob (i), x_common, y_common, boxes, skylines);
    }
  else if (!scm_is_number (me->get_property ("outside-staff-priority"))
	   && !to_boolean (me->get_property ("cross-staff")))
    {
      boxes->push_back (Box (me->extent (x_common, X_AXIS),
			     me->extent (y_common, Y_AXIS)));
    }
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
   If the grob doesn't overlap the right-most affected position, we place it
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

	  if (!b[X_AXIS].is_empty () && !b[Y_AXIS].is_empty ())
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
	      skylines->insert (b, 0, X_AXIS);
	      elements[i]->set_property ("outside-staff-priority", SCM_BOOL_F);
	      last_affected_position[dir] = b[X_AXIS][RIGHT];
	    }

	  /*
	    Ugh: quadratic. --hwn
	   */
	  elements.erase (elements.begin () + i);
	}
    }
}

// TODO: it is tricky to correctly handle skyline placement of cross-staff grobs.
// For example, cross-staff beams cannot be formatted until the distance between
// staves is known and therefore any grobs that depend on the beam cannot be placed
// until the skylines are known. On the other hand, the distance between staves should
// really depend on position of the cross-staff grobs that lie between them.
// Currently, we just leave cross-staff grobs out of the
// skyline altogether, but this could mean that staves are placed so close together
// that there is no room for the cross-staff grob. It also means, of course, that
// we don't get the benefits of skyline placement for cross-staff grobs.
Skyline_pair
Axis_group_interface::skyline_spacing (Grob *me, vector<Grob*> elements)
{
  /* For grobs with an outside-staff-priority, the sorting function might
     call extent and cause suicide. This breaks the contract that is required
     for the STL sort function. To avoid this, we make sure that any suicides
     are triggered beforehand.
  */
  for (vsize i = 0; i < elements.size (); i++)
    if (scm_is_number (elements[i]->get_property ("outside-staff-priority")))
      elements[i]->extent (elements[i], X_AXIS);

  vector_sort (elements, staff_priority_less);
  Grob *x_common = common_refpoint_of_array (elements, me, X_AXIS);
  Grob *y_common = common_refpoint_of_array (elements, me, Y_AXIS);

  assert (y_common == me);

  vsize i = 0;
  vector<Box> boxes;

  Skyline_pair skylines;
  for (i = 0; i < elements.size ()
  	 && !scm_is_number (elements[i]->get_property ("outside-staff-priority")); i++)
    if (!to_boolean (elements[i]->get_property ("cross-staff")))
      add_boxes (elements[i], x_common, y_common, &boxes, &skylines);

  SCM padding_scm = me->get_property ("skyline-horizontal-padding");
  Real padding = robust_scm2double (padding_scm, 0.1);
  skylines.merge (Skyline_pair (boxes, padding, X_AXIS));
  for (; i < elements.size (); i++)
    {
      if (to_boolean (elements[i]->get_property ("cross-staff")))
	continue;

      SCM priority = elements[i]->get_property ("outside-staff-priority");
      vector<Grob*> current_elts;
      current_elts.push_back (elements[i]);
      while (i + 1 < elements.size () 
	     && scm_eq_p (elements[i+1]->get_property ("outside-staff-priority"), priority))
	{
	  if (!to_boolean (elements[i+1]->get_property ("cross-staff")))
	    current_elts.push_back (elements[i+1]);
	  ++i;
	}

      add_grobs_of_one_priority (&skylines, current_elts, x_common, y_common);
    }
  skylines.shift (-me->relative_coordinate (x_common, X_AXIS));
  return skylines;
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, print, 1)
SCM
Axis_group_interface::print (SCM smob)
{
  if (!debug_skylines)
    return SCM_BOOL_F;

  Grob *me = unsmob_grob (smob);
  Stencil ret;
  if (Skyline_pair *s = Skyline_pair::unsmob (me->get_property ("vertical-skylines")))
    {
      ret.add_stencil (Lookup::points_to_line_stencil (0.1, (*s)[UP].to_points (X_AXIS))
		       .in_color (255, 0, 255));
      ret.add_stencil (Lookup::points_to_line_stencil (0.1, (*s)[DOWN].to_points (X_AXIS))
		       .in_color (0, 255, 255));
    }
  return ret.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Axis_group_interface, calc_next_staff_spacing, 1)
SCM
Axis_group_interface::calc_next_staff_spacing (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *grouper = unsmob_grob (me->get_object ("staff-grouper"));

  if (grouper)
    {
      Grob *last_in_group = Staff_grouper_interface::get_last_grob (grouper);
      if (me == last_in_group)
	return grouper->get_property ("after-last-staff-spacing");
      else
	return grouper->get_property ("between-staff-spacing");
    }
  return me->get_property ("default-next-staff-spacing");
}

Real
Axis_group_interface::minimum_distance (Grob *g1, Grob *g2, Axis a)
{
  SCM sym = ly_symbol2scm ((a == Y_AXIS) ? "vertical-skylines" : "horizontal-skylines");

  Skyline_pair *s1 = Skyline_pair::unsmob (g1->get_property (sym));
  Skyline_pair *s2 = Skyline_pair::unsmob (g2->get_property (sym));
  if (s1 && s2)
    return (*s1)[DOWN].distance ((*s2)[UP]);
  return 0;
}

ADD_INTERFACE (Axis_group_interface,
	       "An object that groups other layout objects.",

	       // TODO: some of these properties are specific to
	       // VerticalAxisGroup. We should split off a
	       // vertical-axis-group-interface.
	       /* properties */
	       "X-common "
	       "Y-common "
	       "adjacent-pure-heights "
	       "axes "
	       "default-next-staff-spacing "
	       "elements "
	       "inter-loose-line-spacing "
	       "inter-staff-spacing "
	       "keep-fixed-while-stretching "
	       "max-stretch "
	       "non-affinity-spacing "
	       "next-staff-spacing "
	       "no-alignment "
	       "pure-Y-common "
	       "pure-relevant-items "
	       "pure-relevant-spanners "
	       "staff-affinity "
	       "staff-grouper "
	       "system-Y-offset "
	       "vertical-skylines "
	       );
