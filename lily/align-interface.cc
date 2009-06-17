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
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "skyline-pair.hh"
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

  me->set_property ("positioning-done", SCM_BOOL_T);

  SCM axis = scm_car (me->get_property ("axes"));
  Axis ax = Axis (scm_to_int (axis));

  Align_interface::align_elements_to_extents (me, ax);

  return SCM_BOOL_T;
}

/*
  TODO: This belongs to the old two-pass spacing. Delete me.
*/
MAKE_SCHEME_CALLBACK (Align_interface, stretch_after_break, 1)
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
      Real delta  = extra_space / elems.size () * stacking_dir;
      for (vsize i = 0; i < elems.size (); i++)
	elems[i]->translate_axis (i * delta, Y_AXIS);
    }
  
  return SCM_UNSPECIFIED;
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

	  /* this is perhaps an abuse of minimum-?-extent: maybe we should create
	     another property? But it seems that the only (current) use of
	     minimum-Y-extent is to separate vertically-aligned elements */
	  SCM min_extent = g->get_property (a == X_AXIS
					    ? ly_symbol2scm ("minimum-X-extent")
					    : ly_symbol2scm ("minimum-Y-extent"));

	  if (is_number_pair (min_extent))
	    {
	      Box b;
	      Interval other_extent = g->extent (other_common, other_axis (a));
	      b[a] = ly_scm2interval (min_extent);
	      b[other_axis (a)] = other_extent;
	      if (!other_extent.is_empty ())
		skylines.insert (b, 0, other_axis (a));
	    }

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
	  if (Axis_group_interface::has_interface (g))
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
	me->programming_error ("vertical alignment called before line-breaking");
    }
  
  Direction stacking_dir = robust_scm2dir (me->get_property ("stacking-dir"),
					   DOWN);

  vector<Grob*> elems (all_grobs); // writable copy
  vector<Skyline_pair> skylines;

  get_skylines (me, &elems, a, pure, start, end, &skylines);

  Real where = 0;
  /* TODO: extra-space stuff belongs to two-pass spacing. Delete me */
  SCM extra_space_handle = scm_assq (ly_symbol2scm ("alignment-extra-space"), line_break_details);
  Real extra_space = robust_scm2double (scm_is_pair (extra_space_handle)
					? scm_cdr (extra_space_handle)
					: SCM_EOL,
					0.0);

  Real padding = robust_scm2double (me->get_property ("padding"), 0.0);
  vector<Real> translates;
  Skyline down_skyline (stacking_dir);
  for (vsize j = 0; j < elems.size (); j++)
    {
      Real dy = 0;
      if (j == 0)
	dy = skylines[j][-stacking_dir].max_height ();
      else
	{
	  down_skyline.merge (skylines[j-1][stacking_dir]);
	  dy = down_skyline.distance (skylines[j][-stacking_dir]);
	}

      if (isinf (dy)) /* if the skyline is empty, maybe max_height is infinity_f */
	dy = 0.0;

      dy = max (0.0, dy + padding + extra_space / elems.size ());
      down_skyline.raise (-stacking_dir * dy);
      where += stacking_dir * dy;
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
  Real non_empty_elts = stretchable_children_count (me);
  Real offset = 0.0;
  Direction dir = robust_scm2dir (me->get_property ("stacking-dir"), DOWN);
  for (vsize i = 1; i < elts.size (); i++)
    {
      if (!elts[i]->extent (me, a).is_empty ()
	  && !to_boolean (elts[i]->get_property ("keep-fixed-while-stretching")))
	offset += amount / non_empty_elts;
      elts[i]->translate_axis (dir * offset, a);
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

int
Align_interface::stretchable_children_count (Grob const *me)
{
  extract_grob_set (me, "elements", elts);
  int ret = 0;

  /* start at 1: we will never move the first child while stretching */
  for (vsize i = 1; i < elts.size (); i++)
    if (!to_boolean (elts[i]->get_property ("keep-fixed-while-stretching"))
	&& !elts[i]->extent (elts[i], Y_AXIS).is_empty ())
      ret++;

  return ret;
}

MAKE_SCHEME_CALLBACK (Align_interface, calc_max_stretch, 1)
SCM
Align_interface::calc_max_stretch (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Spanner *spanner_me = dynamic_cast<Spanner*> (me);
  Real ret = 0;

  if (spanner_me && stretchable_children_count (me) > 0)
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
