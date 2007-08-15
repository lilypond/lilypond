/*
  align-interface.cc -- implement Align_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "align-interface.hh"
#include "spanner.hh"
#include "item.hh"
#include "axis-group-interface.hh"
#include "pointer-group-interface.hh"
#include "hara-kiri-group-spanner.hh"
#include "grob-array.hh"
#include "international.hh"
#include "system.hh"
#include "warn.hh"

/*
  TODO: for vertical spacing, should also include a rod & spring
  scheme of sorts into this: the alignment should default to a certain
  distance between element refpoints, unless bbox force a bigger
  distance.
 */

MAKE_SCHEME_CALLBACK (Align_interface, calc_positioning_done, 1);
SCM
Align_interface::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  SCM axis = scm_car (me->get_property ("axes"));
  Axis ax = Axis (scm_to_int (axis));

  SCM force = me->get_property ("forced-distance");
  if (scm_is_number (force))
    Align_interface::align_to_fixed_distance (me, ax);
  else
    Align_interface::align_elements_to_extents (me, ax);

  return SCM_BOOL_T;
}

/*
  merge with align-to-extents?
*/
MAKE_SCHEME_CALLBACK(Align_interface, stretch_after_break, 1)
SCM
Align_interface::stretch_after_break (SCM grob)
{
  Grob *me = unsmob_grob (grob);

  Spanner *me_spanner = dynamic_cast<Spanner *> (me);
  extract_grob_set (me, "elements", elems);

  if (me_spanner && elems.size ())
    {
      Grob *common = common_refpoint_of_array (elems, me, Y_AXIS);

      /* force position callbacks */
      for (vsize i = 0; i < elems.size (); i++)
	elems[i]->relative_coordinate (common, Y_AXIS);

      SCM details =  me_spanner->get_bound (LEFT)->get_property ("line-break-system-details");
      SCM extra_space_handle = scm_assoc (ly_symbol2scm ("fixed-alignment-extra-space"), details);
      
      Real extra_space = robust_scm2double (scm_is_pair (extra_space_handle)
					    ? scm_cdr (extra_space_handle)
					    : SCM_EOL,
					    0.0);

      Direction stacking_dir = robust_scm2dir (me->get_property ("stacking-dir"),
					       DOWN);
      Real delta  = extra_space / elems.size() * stacking_dir;
      for (vsize i = 0; i < elems.size (); i++)
	elems[i]->translate_axis (i * delta, Y_AXIS);
    }
  
  return SCM_UNSPECIFIED;
}

/*
  merge with align-to-extents?
*/
void
Align_interface::align_to_fixed_distance (Grob *me, Axis a)
{
  Direction stacking_dir = robust_scm2dir (me->get_property ("stacking-dir"),
					   DOWN);

  Real dy = robust_scm2double (me->get_property ("forced-distance"), 0.0);

  extract_grob_set (me, "elements", elem_source);

  vector<Grob*> elems (elem_source); // writable..

  Real where = 0;

  Interval v;
  v.set_empty ();
  vector<Real> translates;

  for (vsize j = elems.size (); j--;)
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
	elems.erase (elems.begin () + j);
    }

  for (vsize j = 0; j < elems.size (); j++)
    {
      where += stacking_dir * dy;
      translates.push_back (where);
      v.unite (Interval (where, where));
    }

  /*
    TODO: support self-alignment-{Y, X}
  */
  for (vsize i = 0; i < translates.size (); i++)
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

vector<Real>
Align_interface::get_extents_aligned_translates (Grob *me,
						 vector<Grob*> const &all_grobs,
						 Axis a,
						 bool pure, int start, int end)
{
  Spanner *me_spanner = dynamic_cast<Spanner *> (me);


  SCM line_break_details = SCM_EOL;
  if (a == Y_AXIS && me_spanner)
    {
      if (pure)
	line_break_details = get_root_system (me)->column (start)->get_property ("line-break-system-details");
      else
	line_break_details = me_spanner->get_bound (LEFT)->get_property ("line-break-system-details");

      if (!me->get_system () && !pure)
	me->warning (_ ("vertical alignment called before line-breaking.\n"
			"Only do cross-staff spanners with PianoStaff."));

    }
  
  Direction stacking_dir = robust_scm2dir (me->get_property ("stacking-dir"),
					   DOWN);

  Interval threshold = robust_scm2interval (me->get_property ("threshold"),
					    Interval (0, Interval::infinity ()));

  vector<Interval> dims;
  vector<Grob*> elems;

  for (vsize i = 0; i < all_grobs.size (); i++)
    {
      Interval y = all_grobs[i]->maybe_pure_extent (all_grobs[i], a, pure, start, end);
      if (!y.is_empty ())
	{
	  Grob *e = dynamic_cast<Grob *> (all_grobs[i]);

	  elems.push_back (e);
	  dims.push_back (y);
	}
    }

  /*
    Read self-alignment-X and self-alignment-Y. This may seem like
    code duplication. (and really: it is), but this is necessary to
    prevent ugly cyclic dependencies that arise when you combine
    self-alignment on a child with alignment of children.
  */
  SCM align ((a == X_AXIS)
	     ? me->get_property ("self-alignment-X")
	     : me->get_property ("self-alignment-Y"));

  Interval total;
  Real where = 0;
  Real extra_space = 0.0;
  SCM extra_space_handle = scm_assq (ly_symbol2scm ("alignment-extra-space"), line_break_details);

  extra_space = robust_scm2double (scm_is_pair (extra_space_handle)
				   ? scm_cdr (extra_space_handle)
				   : SCM_EOL,
				   extra_space);

  Real padding = robust_scm2double (me->get_property ("padding"),
				    0.0);
  vector<Real> translates;
  for (vsize j = 0; j < elems.size (); j++)
    {
      Real dy = -dims[j][-stacking_dir];
      if (j)
	dy += dims[j - 1][stacking_dir];

      /*
	we want dy to be > 0
      */
      dy *= stacking_dir;
      if (j)
	dy = min (max (dy, threshold[SMALLER]), threshold[BIGGER]);


      where += stacking_dir * (dy + padding + extra_space / elems.size ());
      total.unite (dims[j] + where);
      translates.push_back (where);
    }

  SCM offsets_handle = scm_assq (ly_symbol2scm ("alignment-offsets"),
				 line_break_details);
  if (scm_is_pair (offsets_handle))
    {
      vsize i = 0;
 
      for (SCM s = scm_cdr (offsets_handle);
	   scm_is_pair (s) && i < translates.size (); s = scm_cdr (s), i++)
	{
	  if (scm_is_number (scm_car (s)))
	    translates[i] = scm_to_double (scm_car (s));
	}
    }

  
  Real center_offset = 0.0;
  
  /*
    also move the grobs that were empty, to maintain spatial order.
  */
  vector<Real> all_translates;
  if (translates.size ())
    {
      Real w = translates[0];

      if (scm_is_number (align))
	center_offset = total.linear_combination (scm_to_double (align));

      for  (vsize i = 0, j = 0; j < all_grobs.size (); j++)
	{
	  if (i < elems.size () && all_grobs[j] == elems[i])
	    w = translates[i++];
	  all_translates.push_back (w - center_offset);
	}
    }
  return all_translates;
}

void
Align_interface::align_elements_to_extents (Grob *me, Axis a)
{
  extract_grob_set (me, "elements", all_grobs);

  vector<Real> translates = get_extents_aligned_translates (me, all_grobs, a, false, 0, 0);
  if (translates.size ())
      for (vsize j = 0; j < all_grobs.size (); j++)
	all_grobs[j]->translate_axis (translates[j], a);
}

Real
Align_interface::get_pure_child_y_translation (Grob *me, Grob *ch, int start, int end)
{
  extract_grob_set (me, "elements", all_grobs);
  SCM dy_scm = me->get_property ("forced-distance");

  if (scm_is_number (dy_scm))
    {
      Real dy = scm_to_double (dy_scm) * robust_scm2dir (me->get_property ("stacking-dir"), DOWN);
      Real pos = 0;
      for (vsize i = 0; i < all_grobs.size (); i++)
	{
	  if (all_grobs[i] == ch)
	    return pos;
	  if (!Hara_kiri_group_spanner::has_interface (all_grobs[i])
	      || !Hara_kiri_group_spanner::request_suicide (all_grobs[i], start, end))
	    pos += dy;
	}
    }
  else
    {
      vector<Real> translates = get_extents_aligned_translates (me, all_grobs, Y_AXIS, true, start, end);

      if (translates.size ())
	{
	  for (vsize i = 0; i < all_grobs.size (); i++)
	    if (all_grobs[i] == ch)
	      return translates[i];
	}
      else
	return 0;
    }

  programming_error (_ ("tried to get a translation for something that isn't my child"));
  return 0;
}

Axis
Align_interface::axis (Grob *me)
{
  return Axis (scm_to_int (scm_car (me->get_property ("axes"))));
}

void
Align_interface::add_element (Grob *me, Grob *element)
{
  Axis a = Align_interface::axis (me);
  SCM sym = axis_offset_symbol (a);
  SCM proc = axis_parent_positioning (a);
    
  element->set_property (sym, proc);
  Axis_group_interface::add_element (me, element);
}

void
Align_interface::set_ordered (Grob *me)
{
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

ADD_INTERFACE (Align_interface,
	       "align-interface",
	       
	       "Order grobs from top to bottom, left to right, right to left or bottom "
	       "to top.  "
	       "For vertical alignments of staves, the @code{break-system-details} of "
	       "the left @internalsref{NonMusicalPaperColumn} may be set to tune vertical spacing "
	       "Set @code{alignment-extra-space} to add extra space for staves. Set "
	       "@code{fixed-alignment-extra-space} to force staves in PianoStaves further apart."
	       ,
	       
	       /*
		 properties
		*/
	       "align-dir "
	       "axes "
	       "elements "
	       "forced-distance "
	       "padding "
	       "positioning-done "
	       "stacking-dir "
	       "threshold "
	       );

struct Foobar
{
  bool has_interface (Grob *);
};

