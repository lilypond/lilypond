/*
  align-interface.cc -- implement Align_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "align-interface.hh"

#include "spanner.hh"
#include "item.hh"
#include "axis-group-interface.hh"
#include "pointer-group-interface.hh"
#include "hara-kiri-group-spanner.hh"
#include "grob-array.hh"

MAKE_SCHEME_CALLBACK (Align_interface, alignment_callback, 2);
SCM
Align_interface::alignment_callback (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis ax = (Axis)scm_to_int (axis);
  Grob *par = me->get_parent (ax);
  if (par && !to_boolean (par->get_property ("positioning-done")))
    Align_interface::align_elements_to_extents (par, ax);
  return scm_from_double (0.0);
}

MAKE_SCHEME_CALLBACK (Align_interface, fixed_distance_alignment_callback, 2);
SCM
Align_interface::fixed_distance_alignment_callback (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis ax = (Axis)scm_to_int (axis);
  Grob *par = me->get_parent (ax);
  if (par && !to_boolean (par->get_property ("positioning-done")))
    Align_interface::align_to_fixed_distance (par, ax);
  return scm_from_double (0.0);
}

/*
  merge with align-to-extents?
*/
void
Align_interface::align_to_fixed_distance (Grob *me, Axis a)
{
  me->set_property ("positioning-done", SCM_BOOL_T);

  SCM d = me->get_property ("stacking-dir");

  Direction stacking_dir = scm_is_number (d) ? to_dir (d) : CENTER;
  if (!stacking_dir)
    stacking_dir = DOWN;

  Real dy = robust_scm2double (me->get_property ("forced-distance"), 0.0);

  extract_grob_set (me, "elements", elem_source);

  Link_array<Grob> elems (elem_source); // writable..

  Real where_f = 0;

  Interval v;
  v.set_empty ();
  Array<Real> translates;

  for (int j = elems.size (); j--;)
    {
      /*
	This is not very elegant, in that we need special support for
	hara-kiri. Unfortunately, the generic wiring of
	force_hara_kiri_callback () (extent and offset callback) is
	such that we might get into a loop if we call extent () or
	offset () the elements.


      */
      if (a == Y_AXIS
	  && Hara_kiri_group_spanner::has_interface (elems[j]))
	Hara_kiri_group_spanner::consider_suicide (elems[j]);

      if (!elems[j]->is_live ())
	elems.del (j);
    }

  for (int j = 0; j < elems.size (); j++)
    {
      where_f += stacking_dir * dy;
      translates.push (where_f);
      v.unite (Interval (where_f, where_f));
    }

  /*
    TODO: support self-alignment-{Y, X}
  */
  for (int i = 0; i < translates.size (); i++)
    elems[i]->translate_axis (translates[i] - v.center (), a);
}

/*
  Hairy function to put elements where they should be. Can be tweaked
  from the outside by setting extra-space in its
  children

  We assume that the children the refpoints of the children are still
  found at 0.0 -- we will fuck up with thresholds if children's
  extents are already moved to locations such as (-16, -8), since the
  dy needed to put things in a row doesn't relate to the distances
  between original refpoints.

  TODO: maybe we should rethink and throw out thresholding altogether.
  The original function has been taken over by
  align_to_fixed_distance ().
*/
void
Align_interface::align_elements_to_extents (Grob *me, Axis a)
{
  Spanner *me_spanner = dynamic_cast<Spanner *> (me);
  if (a == Y_AXIS
      && me_spanner
      && me_spanner->get_bound (LEFT)->break_status_dir () == CENTER)
    {
      me_spanner->warning (_ ("vertical alignment called before line-breaking. Only do cross-staff spanners with PianoStaff."));
    }

  me->set_property ("positioning-done", SCM_BOOL_T);

  SCM d = me->get_property ("stacking-dir");

  Direction stacking_dir = scm_is_number (d) ? to_dir (d) : CENTER;
  if (!stacking_dir)
    stacking_dir = DOWN;

  Interval threshold = robust_scm2interval (me->get_property ("threshold"),
					    Interval (0, Interval::infinity ()));

  Array<Interval> dims;

  Link_array<Grob> elems;

  extract_grob_set (me, "elements", all_grobs);
  for (int i = 0; i < all_grobs.size (); i++)
    {
      Interval y = all_grobs[i]->extent (me, a);
      if (!y.is_empty ())
	{
	  Grob *e = dynamic_cast<Grob *> (all_grobs[i]);

	  elems.push (e);
	  dims.push (y);
	}
    }

  /*
    Read self-alignment-X and self-alignment-Y. This may seem like
    code duplication. (and really: it is), but this is necessary to
    prevent ugly cyclic dependencies that arise when you combine
    self-alignment on a child with alignment of children.
  */
  static SCM prop_syms[2];

  if (!prop_syms[0])
    {
      prop_syms[X_AXIS] = ly_symbol2scm ("self-alignment-X");
      prop_syms[Y_AXIS] = ly_symbol2scm ("self-alignment-Y");
    }

  SCM align (me->internal_get_property (prop_syms[a]));

  Array<Real> translates;
  Interval total;
  Real where_f = 0;

  for (int j = 0; j < elems.size (); j++)
    {
      Real dy = -dims[j][-stacking_dir];
      if (j)
	dy += dims[j - 1][stacking_dir];

      /*
	we want dy to be > 0
      */
      dy *= stacking_dir;
      if (j)
	{
	  dy = min (max (dy, threshold[SMALLER]),
		    threshold[BIGGER]);
	}

      where_f += stacking_dir * dy;
      total.unite (dims[j] + where_f);
      translates.push (where_f);
    }

  Real center_offset = 0.0;
  /*
    also move the grobs that were empty, to maintain spatial order.
  */
  Array<Real> all_translates;
  if (translates.size ())
    {
      int i = 0;
      int j = 0;
      Real w = translates[0];
      while (j < all_grobs.size ())
	{
	  if (i < elems.size () && all_grobs[j] == elems[i])
	    w = translates[i++];
	  all_translates.push (w);
	  j++;
	}

      /*
	FIXME: uncommenting freaks out the Y-alignment of
	line-of-score.
      */
      if (scm_is_number (align))
	center_offset = total.linear_combination (scm_to_double (align));

      for (int j = 0; j < all_grobs.size (); j++)
	all_grobs[j]->translate_axis (all_translates[j] - center_offset, a);
    }
}
Axis
Align_interface::axis (Grob *me)
{
  return Axis (scm_to_int (scm_car (me->get_property ("axes"))));
}

void
Align_interface::add_element (Grob *me, Grob *element, SCM call_back)
{
  element->add_offset_callback (call_back, Align_interface::axis (me));
  Axis_group_interface::add_element (me, element);
}

void
Align_interface::set_axis (Grob *me, Axis a)
{
  Axis_group_interface::set_axes (me, a, a);
  SCM ga_scm = me->get_object ("elements");
  Grob_array *ga = unsmob_grob_array (ga_scm);
  if (!ga)
    {
      ga_scm = Grob_array::make_array ();
      ga = unsmob_grob_array (ga_scm);
      me->set_object ("elements", ga_scm);
    }

  ga->set_ordered (true);
}

/*
  Find Y-axis parent of G that has a #'forced-distance property. This
  has the effect of finding the piano-staff given an object in that
  piano staff.
*/
Grob *
find_fixed_alignment_parent (Grob *g)
{
  while (g)
    {
      if (scm_is_number (g->get_property ("forced-distance")))
	return g;

      g = g->get_parent (Y_AXIS);
    }

  return 0;
}

ADD_INTERFACE (Align_interface, "align-interface",
	       "Order grobs from top to bottom, left to right, right to left or bottom"
	       "to top.",
	       "forced-distance stacking-dir align-dir threshold positioning-done "
	       "elements axes");

struct Foobar
{
  bool has_interface (Grob *);
};

