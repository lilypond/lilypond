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
#include "paper-column.hh"

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

      SCM details = me_spanner->get_bound (LEFT)->get_property ("line-break-system-details");
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

  for (vsize i = 0; i < translates.size (); i++)
    elems[i]->translate_axis (translates[i] - v.center (), a);
}

/* for each grob, find its upper and lower skylines. If the grob has
   an empty extent, delete it from the list instead. If the extent is
   non-empty but there is no skyline available (or pure is true), just
   create a flat skyline from the bounding box */
static void
get_skylines (Grob *me,
	      vector<Grob*> *const elements,
	      Axis a,
	      bool pure, int start, int end,
	      vector<Skyline_pair> *const ret)
{
  /* each child's skyline was calculated according to the common refpoint of its
     elements. Here we need all the skylines to be positioned with respect to
     a single refpoint, so we need the common refpoint of the common refpoints
     of the elements of the children */
  vector<Grob*> child_refpoints;
  for (vsize i = 0; i < elements->size (); i++)
    {
      Grob *elt = (*elements)[i];
      Grob *child_common = unsmob_grob ((a == Y_AXIS)
					? elt->get_object ("X-common")
					: elt->get_object ("Y-common"));
      
      if (!child_common)
	{
	  extract_grob_set (elt, "elements", child_elts);
	  child_common = common_refpoint_of_array (child_elts, elt, other_axis (a));
	}
      
      child_refpoints.push_back (child_common);
    }
  Grob *common_refpoint = common_refpoint_of_array (child_refpoints, me, other_axis (a));
  
  for (vsize i = elements->size (); i--;)
    {
      Grob *g = (*elements)[i];
      Interval extent = g->maybe_pure_extent (g, a, pure, start, end);
      Interval other_extent = pure ? Interval (-infinity_f, infinity_f)
	: g->extent (common_refpoint, other_axis (a));
      Box b;
      b[a] = extent;
      b[other_axis (a)] = other_extent;

      if (extent.is_empty ())
	{
	  elements->erase (elements->begin () + i);
	  continue;
	}

      Skyline_pair skylines;
      if (!pure
	  && Skyline_pair::unsmob (g->get_property ("skylines")))
	skylines = *Skyline_pair::unsmob (g->get_property ("skylines"));
      else
	{
	  if (!pure)
	    programming_error ("no skylines for alignment-child\n");
	  
	  skylines = Skyline_pair (b, 0, other_axis (a));
	}

      /* each skyline is calculated relative to (potentially) a different other_axis
	 coordinate. In order to compare the skylines effectively, we need to shift them
	 to some absolute reference point */
      if (!pure)
	{
	  /* this is perhaps an abuse of minimum-?-extent: maybe we should create
	     another property? But it seems that the only (current) use of
	     minimum-Y-extent is to separate vertically-aligned elements */
	  SCM min_extent = g->get_property (a == X_AXIS ? "minimum-X-extent" : "minimum-Y-extent");
	  if (is_number_pair (min_extent))
	    {
	      b[a] = ly_scm2interval (min_extent);
	      skylines.insert (b, 0, other_axis (a));
	    }

	  Real offset = child_refpoints[i]->relative_coordinate (common_refpoint, other_axis (a));
	  skylines.shift (offset);
	}


      ret->push_back (skylines);
    }
  reverse (*ret);
}

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

  vector<Grob*> elems (all_grobs); // writable copy
  vector<Skyline_pair> skylines;

  get_skylines (me, &elems, a, pure, start, end, &skylines);

  Real where = 0;
  SCM extra_space_handle = scm_assq (ly_symbol2scm ("alignment-extra-space"), line_break_details);
  Real extra_space = robust_scm2double (scm_is_pair (extra_space_handle)
					? scm_cdr (extra_space_handle)
					: SCM_EOL,
					0.0);

  Real padding = robust_scm2double (me->get_property ("padding"), 0.0);
  vector<Real> translates;
  for (vsize j = 0; j < elems.size (); j++)
    {
      Real dy = 0;
      if (j == 0)
	dy = skylines[j][-stacking_dir].max_height ();
      else
	dy = skylines[j-1][stacking_dir].distance (skylines[j][-stacking_dir]);

      where += stacking_dir * max (0.0, dy + padding + extra_space / elems.size ());
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

  vector<Real> all_translates;

  if (!translates.empty ())
    {
      Real w = translates[0];
      for  (vsize i = 0, j = 0; j < all_grobs.size (); j++)
	{
	  if (i < elems.size () && all_grobs[j] == elems[i])
	    w = translates[i++];
	  all_translates.push_back (w);
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

/* After we have already determined the y-offsets of our children, we may still
   want to stretch them a little. */
void
Align_interface::stretch (Grob *me, Real amount, Axis a)
{
  extract_grob_set (me, "elements", elts);
  Real non_empty_elts = 0.0;
  for (vsize i = 0; i < elts.size (); i++)
    non_empty_elts += !elts[i]->extent (me, a).is_empty ();

  Real offset = 0.0;
  Direction dir = robust_scm2dir (me->get_property ("stacking-dir"), DOWN);
  for (vsize i = 0; i < elts.size (); i++)
    {
      elts[i]->translate_axis (dir * offset, a);
      if (!elts[i]->extent (me, a).is_empty ())
	offset += amount / non_empty_elts;
    }
  me->flush_extent_cache (Y_AXIS);
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

  programming_error (_ ("tried to get a translation for something that is no child of mine"));
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

MAKE_SCHEME_CALLBACK (Align_interface, calc_max_stretch, 1)
SCM
Align_interface::calc_max_stretch (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Spanner *spanner_me = dynamic_cast<Spanner*> (me);
  Real ret = 0;

  if (spanner_me)
    {
      Paper_column *left = dynamic_cast<Paper_column*> (spanner_me->get_bound (LEFT));
      Real height = me->extent (me, Y_AXIS).length ();
      SCM line_break_details = left->get_property ("line-break-system-details");
      SCM fixed_offsets = scm_assq (ly_symbol2scm ("alignment-offsets"),
				    line_break_details);

      /* if there are fixed offsets, we refuse to stretch */
      if (fixed_offsets != SCM_BOOL_F)
	ret = 0;
      else
	ret = height * height / 80.0; /* why this, exactly? -- jneem */
    }
  return scm_from_double (ret);
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
