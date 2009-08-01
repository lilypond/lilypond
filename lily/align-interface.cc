/*
  align-interface.cc -- implement Align_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "grob-array.hh"
#include "hara-kiri-group-spanner.hh"
#include "international.hh"
#include "item.hh"
#include "page-layout-problem.hh"
#include "paper-book.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "skyline-pair.hh"
#include "system.hh"
#include "warn.hh"


MAKE_SCHEME_CALLBACK (Align_interface, align_to_minimum_distances, 1);
SCM
Align_interface::align_to_minimum_distances (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  me->set_property ("positioning-done", SCM_BOOL_T);

  SCM axis = scm_car (me->get_property ("axes"));
  Axis ax = Axis (scm_to_int (axis));

  Align_interface::align_elements_to_minimum_distances (me, ax);

  return SCM_BOOL_T;
}

MAKE_SCHEME_CALLBACK (Align_interface, align_to_ideal_distances, 1);
SCM
Align_interface::align_to_ideal_distances (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  me->set_property ("positioning-done", SCM_BOOL_T);

  Align_interface::align_elements_to_ideal_distances (me);

  return SCM_BOOL_T;
}

/* for each grob, find its upper and lower skylines. If the grob has
   an empty extent, delete it from the list instead. If the extent is
   non-empty but there is no skyline available (or pure is true), just
   create a flat skyline from the bounding box */
// TODO(jneem): the pure and non-pure parts seem to share very little
// code. Split them into 2 functions, perhaps?
static void
get_skylines (Grob *me,
	      vector<Grob*> *const elements,
	      Axis a,
	      bool pure, int start, int end,
	      vector<Skyline_pair> *const ret)
{
  Grob *other_common = common_refpoint_of_array (*elements, me, other_axis (a));
  
  for (vsize i = elements->size (); i--;)
    {
      Grob *g = (*elements)[i];
      Skyline_pair skylines;

      if (!pure)
	{
	  Skyline_pair *skys = Skyline_pair::unsmob (g->get_property (a == Y_AXIS
								      ? "vertical-skylines"
								      : "horizontal-skylines"));
	  if (skys)
	    skylines = *skys;

	  /* This skyline was calculated relative to the grob g. In order to compare it to
	     skylines belonging to other grobs, we need to shift it so that it is relative
	     to the common reference. */
	  Real offset = g->relative_coordinate (other_common, other_axis (a));
	  skylines.shift (offset);
	}
      else
	{
	  assert (a == Y_AXIS);
	  Interval extent = g->pure_height (g, start, end);
	  if (!extent.is_empty ())
	    {
	      Box b;
	      b[a] = extent;
	      b[other_axis (a)] = Interval (0, infinity_f);
	      skylines.insert (b, 0, other_axis (a));
	    }

	  // This is a hack to get better accuracy on the pure-height of VerticalAlignment.
	  // It's quite common for a treble clef to be the highest element of one system
	  // and for a low note (or lyrics) to be the lowest note on another. The two will
	  // never collide, but the pure-height stuff only works with bounding boxes, so it
	  // doesn't know that. The result is a significant over-estimation of the pure-height,
	  // especially on systems with many staves. To correct for this, we build a skyline
	  // in two parts: the part we did above contains most of the grobs (note-heads, etc.)
	  // while the bit we're about to do only contains the breakable grobs at the beginning
	  // of the system. This way, the tall treble clefs are only compared with the treble
	  // clefs of the other staff and they will be ignored if the staff above is, for example,
	  // lyrics.
	  if (Axis_group_interface::has_interface (g)
	      && !Hara_kiri_group_spanner::request_suicide (g, start, end))
	    {
	      Interval begin_of_line_extent = Axis_group_interface::begin_of_line_pure_height (g, start);
	      if (!begin_of_line_extent.is_empty ())
		{
		  Box b;
		  b[a] = begin_of_line_extent;
		  b[other_axis (a)] = Interval (-infinity_f, -1);
		  skylines.insert (b, 0, other_axis (a));
		}
	    }
	}

      if (skylines.is_empty ())
	elements->erase (elements->begin () + i);
      else
	ret->push_back (skylines);
    }
  reverse (*ret);
}

vector<Real>
Align_interface::get_minimum_translations (Grob *me,
					   vector<Grob*> const &all_grobs,
					   Axis a,
					   bool pure, int start, int end)
{
  if (!pure && a == Y_AXIS && dynamic_cast<Spanner*> (me) && !me->get_system ())
    me->programming_error ("vertical alignment called before line-breaking");
  
  Direction stacking_dir = robust_scm2dir (me->get_property ("stacking-dir"),
					   DOWN);
  vector<Grob*> elems (all_grobs); // writable copy
  vector<Skyline_pair> skylines;

  get_skylines (me, &elems, a, pure, start, end, &skylines);

  SCM forced_distances = ly_assoc_get (ly_symbol2scm ("alignment-distances"),
				       Page_layout_problem::get_details (me),
				       SCM_EOL);

  Real where = 0;
  Real default_padding = robust_scm2double (me->get_property ("padding"), 0.0);
  vector<Real> translates;
  Skyline down_skyline (stacking_dir);
  SCM last_spaceable_element_details = SCM_EOL;
  Real last_spaceable_element_pos = 0;
  bool found_spaceable_element = false;
  for (vsize j = 0; j < elems.size (); j++)
    {
      Real dy = 0;
      Real padding = default_padding;

      if (j == 0)
	dy = skylines[j][-stacking_dir].max_height ();
      else
	{
	  down_skyline.merge (skylines[j-1][stacking_dir]);
	  dy = down_skyline.distance (skylines[j][-stacking_dir]);
	}

      if (isinf (dy)) /* if the skyline is empty, maybe max_height is infinity_f */
	dy = 0.0;

      if (Page_layout_problem::is_spaceable (elems[j]))
	{
	  Real min_distance = 0;
	  Page_layout_problem::read_spacing_spec (last_spaceable_element_details,
						  &padding,
						  ly_symbol2scm ("padding"));
	  if (Page_layout_problem::read_spacing_spec (last_spaceable_element_details,
						      &min_distance,
						      ly_symbol2scm ("minimum-distance")))
	    dy = max (dy, min_distance + stacking_dir*(last_spaceable_element_pos - where));

	  if (found_spaceable_element && scm_is_pair (forced_distances))
	    {
	      SCM forced_dist = scm_car (forced_distances);
	      forced_distances = scm_cdr (forced_distances);

	      if (scm_is_number (forced_dist))
		dy = scm_to_double (forced_dist) + stacking_dir * (last_spaceable_element_pos - where);
	    }
	  last_spaceable_element_details = elems[j]->get_property ("next-staff-spacing");
	  found_spaceable_element = true;
	}
      else
	{
	  // TODO: provide support for min-distance and padding for non-spaceable elements also.
	}

      dy = max (0.0, dy + padding);
      down_skyline.raise (-stacking_dir * dy);
      where += stacking_dir * dy;
      translates.push_back (where);

      if (Page_layout_problem::is_spaceable (elems[j]))
	last_spaceable_element_pos = where;
    }

  // So far, we've computed the translates for all the non-empty elements.
  // Here, we set the translates for the empty elements: an empty element
  // gets the same translation as the last non-empty element before it.
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
Align_interface::align_elements_to_ideal_distances (Grob *me)
{
  System *sys = me->get_system ();
  Page_layout_problem layout (NULL, SCM_EOL, scm_list_1 (sys->self_scm ()));

  layout.solution (true);
}

void
Align_interface::align_elements_to_minimum_distances (Grob *me, Axis a)
{
  extract_grob_set (me, "elements", all_grobs);

  vector<Real> translates = get_minimum_translations (me, all_grobs, a, false, 0, 0);
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
      vector<Real> translates = get_minimum_translations (me, all_grobs, Y_AXIS, true, start, end);

      if (translates.size ())
	{
	  for (vsize i = 0; i < all_grobs.size (); i++)
	    if (all_grobs[i] == ch)
	      return translates[i];
	}
      else
	return 0;
    }

  programming_error ("tried to get a translation for something that is no child of mine");
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

ADD_INTERFACE (Align_interface,
	       "Order grobs from top to bottom, left to right, right to left"
	       " or bottom to top.  For vertical alignments of staves, the"
	       " @code{break-system-details} of the left"
	       " @rinternals{NonMusicalPaperColumn} may be set to tune"
	       " vertical spacing.  Set @code{alignment-extra-space} to add"
	       " extra space for staves.  Set"
	       " @code{fixed-alignment-extra-space} to force staves in"
	       " @code{PianoStaff}s further apart.",
	       
	       /* properties */
	       "align-dir "
	       "axes "
	       "elements "
	       "padding "
	       "positioning-done "
	       "stacking-dir "
	       "threshold "
	       );
