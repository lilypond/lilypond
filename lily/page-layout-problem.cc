/*
  page-layout-problem.cc -- space systems nicely on a page. If systems can
  be stretched, do that too.

  source file of the GNU LilyPond music typesetter

  (c) 2009 Joe Neeman <joeneeman@gmail.com>
*/

#include "page-layout-problem.hh"

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "hara-kiri-group-spanner.hh"
#include "international.hh"
#include "item.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "prob.hh"
#include "skyline-pair.hh"
#include "system.hh"

Page_layout_problem::Page_layout_problem (Paper_book *pb, SCM page_scm, SCM systems)
  : bottom_skyline_ (DOWN)
{
  Prob *page = unsmob_prob (page_scm);
  header_height_ = 0;
  footer_height_ = 0;
  header_padding_ = 0;
  footer_padding_ = 0;
  page_height_ = 100;

  if (page)
    {
      Stencil *head = unsmob_stencil (page->get_property ("head-stencil"));
      Stencil *foot = unsmob_stencil (page->get_property ("foot-stencil"));

      header_height_ = head ? head->extent (Y_AXIS).length () : 0;
      footer_height_ = foot ? foot->extent (Y_AXIS).length () : 0;
      page_height_ = robust_scm2double (page->get_property ("paper-height"), 100);
    }

  // Initially, bottom_skyline_ represents the top of the page. Make
  // it solid, so that the top of the first system will be forced
  // below the top of the printable area.
  bottom_skyline_.set_minimum_height (-header_height_);

  SCM between_system_spacing = SCM_EOL;
  SCM between_scores_system_spacing = SCM_EOL;
  SCM after_title_spacing = SCM_EOL;
  SCM before_title_spacing = SCM_EOL;
  SCM between_title_spacing = SCM_EOL;

  // top_system_spacing controls the spring from the top of the printable
  // area to the first staff. It allows the user to control the offset of
  // the first staff (as opposed to the top of the first system) from the
  // top of the page. Similarly for bottom_system_spacing.
  SCM top_system_spacing = SCM_EOL;
  SCM bottom_system_spacing = SCM_EOL;
  if (pb && pb->paper_)
    {
      Output_def *paper = pb->paper_;
      between_system_spacing = paper->c_variable ("between-system-spacing");
      between_scores_system_spacing = paper->c_variable ("between-scores-system-spacing");
      after_title_spacing = paper->c_variable ("after-title-spacing");
      before_title_spacing = paper->c_variable ("before-title-spacing");
      between_title_spacing = paper->c_variable ("between-title-spacing");
      bottom_system_spacing = paper->c_variable ("bottom-system-spacing");
      top_system_spacing = paper->c_variable ("top-system-spacing");
      if (scm_is_pair (systems) && unsmob_prob (scm_car (systems)))
	top_system_spacing = paper->c_variable ("top-title-spacing");

      // Note: the page height here does _not_ reserve space for headers and
      // footers. This is because we want to anchor the top-system-spacing
      // spring at the _top_ of the header.
      page_height_ -= robust_scm2double (paper->c_variable ("top-margin"), 0)
	+ robust_scm2double (paper->c_variable ("bottom-margin"), 0);

      read_spacing_spec (top_system_spacing, &header_padding_, ly_symbol2scm ("padding"));
      read_spacing_spec (bottom_system_spacing, &footer_padding_, ly_symbol2scm ("padding"));
    }
  bool last_system_was_title = false;


  for (SCM s = systems; scm_is_pair (s); s = scm_cdr (s))
    {
      bool first = (s == systems);

      if (Grob *g = unsmob_grob (scm_car (s)))
	{
	  System *sys = dynamic_cast<System*> (g);
	  if (!sys)
	    {
	      programming_error ("got a grob for vertical spacing that wasn't a System");
	      continue;
	    }

	  SCM spec = between_system_spacing;
	  if (first)
	    spec = top_system_spacing;
	  else if (last_system_was_title)
	    spec = after_title_spacing;
	  else if (0 == Paper_column::get_rank (sys->get_bound (LEFT)))
	    spec = between_scores_system_spacing;

	  Spring spring (first ? 0 : 1, 0.0);
	  Real padding = 0.0;
	  alter_spring_from_spacing_spec (spec, &spring);
	  read_spacing_spec (spec, &padding, ly_symbol2scm ("padding"));

	  append_system (sys, spring, padding);
	  last_system_was_title = false;
	}
      else if (Prob *p = unsmob_prob (scm_car (s)))
	{
	  SCM spec = first ? top_system_spacing
	    : (last_system_was_title ? between_title_spacing : before_title_spacing);
	  Spring spring (first ? 0 : 1, 0.0);
	  Real padding = 0.0;
	  alter_spring_from_spacing_spec (spec, &spring);
	  read_spacing_spec (spec, &padding, ly_symbol2scm ("padding"));

	  append_prob (p, spring, padding);
	  last_system_was_title = true;
	}
      else
	programming_error ("got a system that was neither a Grob nor a Prob");
    }

  Spring last_spring (0, 0);
  Real last_padding = 0;
  alter_spring_from_spacing_spec (bottom_system_spacing, &last_spring);
  read_spacing_spec (bottom_system_spacing, &last_padding, ly_symbol2scm ("padding"));
  last_spring.ensure_min_distance (last_padding - bottom_skyline_.max_height () + footer_height_);
  springs_.push_back (last_spring);

  if (elements_.size ())
    {
      Real bottom_padding = 0;

      // TODO: junk bottom-space now that we have bottom-system-spacing?
      // bottom-space has the flexibility that one can do it per-system.
      // NOTE: bottom-space is misnamed since it is not stretchable space.
      if (Prob *p = elements_.back ().prob)
	bottom_padding = robust_scm2double (p->get_property ("bottom-space"), 0);
      else if (elements_.back ().staves.size ())
	{
	  SCM details = get_details (elements_.back ());
	  bottom_padding = robust_scm2double (ly_assoc_get (ly_symbol2scm ("bottom-space"),
							    details,
							    SCM_BOOL_F),
					      0.0);
	}
      page_height_ -= bottom_padding;
    }
}

void
Page_layout_problem::set_header_height (Real height)
{
  header_height_ = height;
}

void
Page_layout_problem::set_footer_height (Real height)
{
  footer_height_ = height;
}

void
Page_layout_problem::append_system (System *sys, Spring const& spring, Real padding)
{
  Grob *align = sys->get_vertical_alignment ();
  if (!align)
    return;

  align->set_property ("positioning-done", SCM_BOOL_T);

  extract_grob_set (align, "elements", all_elts);
  vector<Grob*> elts = filter_dead_elements (all_elts);
  vector<Real> minimum_offsets = Align_interface::get_minimum_translations (align, elts, Y_AXIS,
									    false, 0, 0);

  Skyline up_skyline (UP);
  Skyline down_skyline (DOWN);
  build_system_skyline (elts, minimum_offsets, &up_skyline, &down_skyline);

  Real minimum_distance = up_skyline.distance (bottom_skyline_) + padding;

  Spring spring_copy = spring;
  spring_copy.ensure_min_distance (minimum_distance);
  springs_.push_back (spring_copy);

  bottom_skyline_ = down_skyline;
  elements_.push_back (Element (elts, minimum_offsets));

  // Add the springs for the VerticalAxisGroups in this system.

  // If the user has specified the offsets of the individual staves, fix the
  // springs at the given distances. Otherwise, use stretchable springs.
  SCM details = get_details (elements_.back ());
  SCM manual_dists = ly_assoc_get (ly_symbol2scm ("alignment-distances"), details, SCM_EOL);
  vsize last_spaceable_staff = 0;
  bool found_spaceable_staff = false;
  for (vsize i = 0; i < elts.size (); ++i)
    {
      if (is_spaceable (elts[i]))
	{
	  // We don't add a spring for the first staff, since
	  // we are only adding springs _between_ staves here.
	  if (!found_spaceable_staff)
	    {
	      found_spaceable_staff = true;
	      last_spaceable_staff = i;
	      continue;
	    }

	  Spring spring (0.5, 0.0);
	  SCM spec = elts[last_spaceable_staff]->get_property ("next-staff-spacing");
	  alter_spring_from_spacing_spec (spec, &spring);

	  springs_.push_back (spring);
	  Real min_distance = (found_spaceable_staff ? minimum_offsets[last_spaceable_staff] : 0) - minimum_offsets[i];
	  springs_.back ().ensure_min_distance (min_distance);

	  if (scm_is_pair (manual_dists))
	    {
	      if (scm_is_number (scm_car (manual_dists)))
		{
		  Real dy = scm_to_double (scm_car (manual_dists));

		  springs_.back ().set_distance (dy);
		  springs_.back ().set_min_distance (dy);
		  springs_.back ().set_inverse_stretch_strength (0);
		}
	      manual_dists = scm_cdr (manual_dists);
	    }
	  last_spaceable_staff = i;
	}
    }

  // Corner case: there was only one staff, and it wasn't spaceable.
  // Mark it spaceable, because we do not allow non-spaceable staves
  // to be at the top or bottom of a system.
  if (!found_spaceable_staff && elts.size ())
    mark_as_spaceable (elts[0]);
}

void
Page_layout_problem::append_prob (Prob *prob, Spring const& spring, Real padding)
{
  Skyline_pair *sky = Skyline_pair::unsmob (prob->get_property ("vertical-skylines"));
  Real minimum_distance = 0;
  if (sky)
    {
      minimum_distance = (*sky)[UP].distance (bottom_skyline_);
      bottom_skyline_ = (*sky)[DOWN];
    }
  else if (Stencil *sten = unsmob_stencil (prob->get_property ("stencil")))
    {
      Interval iv = sten->extent (Y_AXIS);
      minimum_distance = iv[UP] - bottom_skyline_.max_height ();

      bottom_skyline_.clear ();
      bottom_skyline_.set_minimum_height (iv[DOWN]);
    }
  minimum_distance += padding;

  Spring spring_copy = spring;
  spring_copy.ensure_min_distance (minimum_distance);
  springs_.push_back (spring_copy);
  elements_.push_back (Element (prob));
}

void
Page_layout_problem::solve_rod_spring_problem (bool ragged)
{
  Simple_spacer spacer;

  for (vsize i = 0; i < springs_.size (); ++i)
    spacer.add_spring (springs_[i]);

  spacer.solve (page_height_, ragged);
  solution_ = spacer.spring_positions ();
}

// The solution_ vector stores the position of every live VerticalAxisGroup
// and every title. From that information,
// 1) within each system, stretch the staves so they land at the right position
// 2) find the offset of each system (relative to the printable area of the page).
// TODO: this function is getting too long, maybe split it up?
SCM
Page_layout_problem::find_system_offsets ()
{
  SCM system_offsets = SCM_EOL;
  SCM *tail = &system_offsets;

  // spring_idx 0 is the top of the page. Interesting values start from 1.
  vsize spring_idx = 1;
  vector<Grob*> loose_lines;
  vector<Real> loose_line_min_distances;
  Grob *last_spaceable_line = 0;
  Real last_spaceable_line_translation = 0;
  for (vsize i = 0; i < elements_.size (); ++i)
    {
      if (elements_[i].prob)
	{
	  *tail = scm_cons (scm_from_double (solution_[spring_idx]), SCM_EOL);
	  tail = SCM_CDRLOC (*tail);

	  // Lay out any non-spaceable lines between this line and
	  // the last one.
	  if (loose_lines.size ())
	    {
	      Interval loose_extent = loose_lines.back ()->extent (loose_lines.back (), Y_AXIS);
	      Interval prob_extent = unsmob_stencil (elements_[i].prob->get_property ("stencil"))->extent (Y_AXIS);
	      Real min_distance = -loose_extent[DOWN] + prob_extent[UP]; // TODO: include padding/minimum-distance

	      loose_line_min_distances.push_back (min_distance);
	      loose_lines.push_back (0);

	      distribute_loose_lines (loose_lines, loose_line_min_distances,
				      last_spaceable_line_translation, -solution_[spring_idx]);
	      loose_lines.clear ();
	      loose_line_min_distances.clear ();
	    }

	  last_spaceable_line = 0;
	  last_spaceable_line_translation = -solution_[spring_idx];
	  spring_idx++;
	}
      else
	{
	  // Getting this signs right here is a little tricky. The configuration
	  // we return has zero at the top of the page and positive numbers further
	  // down, as does the solution_ vector.  Within a staff, however, positive
	  // numbers are up.
	  // TODO: perhaps change the way the page 'configuration variable works so
	  // that it is consistent with the usual up/down sign conventions in
	  // Lilypond. Then this would be less confusing.

	  // These two positions are relative to the page (with positive numbers being
	  // down).
	  Real first_staff_position = solution_[spring_idx];
	  Real first_staff_min_translation = elements_[i].min_offsets.size () ? elements_[i].min_offsets[0] : 0;
	  Real system_position = first_staff_position + first_staff_min_translation;

	  // Position the staves within this system.
	  Real translation = 0;
	  vector<Real> const& min_offsets = elements_[i].min_offsets;
	  bool found_spaceable_staff = false;
	  for (vsize staff_idx = 0; staff_idx < elements_[i].staves.size (); ++staff_idx)
	    {
	      Grob *staff = elements_[i].staves[staff_idx];
	      staff->set_property ("system-Y-offset", scm_from_double (-system_position));

	      if (is_spaceable (staff))
		{
		  // this is relative to the system: negative numbers are down.
		  translation = system_position - solution_[spring_idx];
		  spring_idx++;

		  // Lay out any non-spaceable lines between this line and
		  // the last one.
		  if (loose_lines.size ())
		    {
		      loose_line_min_distances.push_back (min_offsets[staff_idx-1] - min_offsets[staff_idx]);
		      loose_lines.push_back (staff);

		      distribute_loose_lines (loose_lines, loose_line_min_distances,
					      last_spaceable_line_translation, translation - system_position);
		      loose_lines.clear ();
		      loose_line_min_distances.clear ();
		    }
		  last_spaceable_line = staff;
		  // Negative is down but the translation is relative to the whole page.
		  last_spaceable_line_translation = -system_position + translation;

		  staff->translate_axis (translation, Y_AXIS);
		  found_spaceable_staff = true;
		}
	      else
		{
		  if (loose_lines.empty ())
		    loose_lines.push_back (last_spaceable_line);

		  loose_lines.push_back (staff);
		  if (staff_idx)
		    loose_line_min_distances.push_back (min_offsets[staff_idx-1] - min_offsets[staff_idx]);
		  else
		    {
		      Real min_dist = 0;
		      if (last_spaceable_line)
			min_dist = Axis_group_interface::minimum_distance (last_spaceable_line,
									   staff,
									   Y_AXIS);
		      else // distance to the top margin
			min_dist = header_padding_ + header_height_ + staff->extent (staff, Y_AXIS)[UP];

		      loose_line_min_distances.push_back (min_dist);
		    }
		}
	    }

	  // Corner case: even if a system has no live staves, it still takes up
	  // one spring (a system with one live staff also takes up one spring),
	  // which we need to increment past.
	  if (!found_spaceable_staff)
	    spring_idx++;

	  *tail = scm_cons (scm_from_double (system_position), SCM_EOL);
	  tail = SCM_CDRLOC (*tail);
	}
    }

  if (loose_lines.size ())
    {
      Grob *last = loose_lines.back ();
      Interval last_ext = last->extent (last, Y_AXIS);
      loose_line_min_distances.push_back (-last_ext[DOWN] + footer_height_ + footer_padding_);
      loose_lines.push_back (0);

      distribute_loose_lines (loose_lines, loose_line_min_distances,
			      last_spaceable_line_translation, -page_height_);

    }

  assert (spring_idx == solution_.size () - 1);
  return system_offsets;
}

// Given two lines that are already spaced (the first and last
// elements of loose_lines), distribute some unspaced lines between
// them.
void
Page_layout_problem::distribute_loose_lines (vector<Grob*> const &loose_lines,
					     vector<Real> const &min_distances,
					     Real first_translation, Real last_translation)
{
  Simple_spacer spacer;
  for (vsize i = 0; i + 1 < loose_lines.size (); ++i)
    {
      SCM spec = get_spacing_spec (loose_lines[i], loose_lines[i+1]);
      Spring spring (1.0, 0.0);
      alter_spring_from_spacing_spec (spec, &spring);
      spring.ensure_min_distance (min_distances[i]);
      spacer.add_spring (spring);
    }

  // Remember: offsets are decreasing, since we're going from UP to DOWN!
  spacer.solve (first_translation - last_translation, false);

  vector<Real> solution = spacer.spring_positions ();
  for (vsize i = 1; i + 1 < solution.size (); ++i)
    {
      Real system_offset = scm_to_double (loose_lines[i]->get_property ("system-Y-offset"));
      loose_lines[i]->translate_axis (first_translation - solution[i] - system_offset, Y_AXIS);
    }
}

SCM
Page_layout_problem::solution (bool ragged)
{
  solve_rod_spring_problem (ragged);
  return find_system_offsets ();
}

// Build upper and lower skylines for a system. We don't yet know the positions
// of the staves within the system, so we make the skyline as conservative as
// possible. That is, for the upper skyline, we pretend that all of the staves
// in the system are packed together close to the top system; for the lower
// skyline, we pretend that all of the staves are packed together close to
// the bottom system.
//
// The upper skyline is relative to the top staff; the lower skyline is relative to
// the bottom staff.
void
Page_layout_problem::build_system_skyline (vector<Grob*> const& staves,
					   vector<Real> const& minimum_translations,
					   Skyline *up,
					   Skyline *down)
{
  if (minimum_translations.empty ())
    return;

  assert (staves.size () == minimum_translations.size ());
  Real first_translation = minimum_translations[0];
  Real last_spaceable_dy = 0;
  Real first_spaceable_dy = 0;
  bool found_spaceable_staff = false;

  for (vsize i = 0; i < staves.size (); ++i)
    {
      Real dy = minimum_translations[i] - first_translation;
      Grob *g = staves[i];
      Skyline_pair *sky = Skyline_pair::unsmob (g->get_property ("vertical-skylines"));
      if (sky)
	{
	  up->raise (-dy);
	  up->merge ((*sky)[UP]);
	  up->raise (dy);

	  down->raise (-dy);
	  down->merge ((*sky)[DOWN]);
	  down->raise (dy);
	}
      if (is_spaceable (staves[i]))
	{
	  if (!found_spaceable_staff)
	    {
	      found_spaceable_staff = true;
	      first_spaceable_dy = dy;
	    }
	  last_spaceable_dy = dy;
	}
    }

  // Leave the up skyline at a position relative
  // to the top spaceable staff.
  up->raise (-first_spaceable_dy);

  // Leave the down skyline at a position
  // relative to the bottom spaceable staff.
  down->raise (-last_spaceable_dy);
}

Interval
Page_layout_problem::prob_extent (Prob *p)
{
  Stencil *sten = unsmob_stencil (p->get_property ("stencil"));
  return sten ? sten->extent (Y_AXIS) : Interval (0, 0);
}

Interval
Page_layout_problem::first_staff_extent (Element const& e)
{
  if (e.prob)
    return prob_extent (e.prob);
  else if (e.staves.size ())
    return e.staves[0]->extent (e.staves[0], Y_AXIS);

  return Interval (0, 0);
}

Interval
Page_layout_problem::last_staff_extent (Element const& e)
{
  if (e.prob)
    return prob_extent (e.prob);
  else if (e.staves.size ())
    return e.staves.back ()->extent (e.staves.back (), Y_AXIS);

  return Interval (0, 0);
}

SCM
Page_layout_problem::get_details (Element const& elt)
{
  if (elt.staves.empty ())
    return SCM_EOL;

  return get_details (elt.staves.back ()->get_system ());
}

SCM
Page_layout_problem::get_details (Grob *g)
{
  Grob *left_bound = dynamic_cast<Spanner*> (g)->get_bound (LEFT);
  return left_bound->get_property ("line-break-system-details");
}

bool
Page_layout_problem::is_spaceable (Grob *g)
{
  return !scm_is_number (g->get_property ("staff-affinity"));
}

void
Page_layout_problem::mark_as_spaceable (Grob *g)
{
  g->set_property ("staff-affinity", SCM_BOOL_F);
}

bool
Page_layout_problem::read_spacing_spec (SCM spec, Real* dest, SCM sym)
{
  SCM pair = scm_sloppy_assq (sym, spec);
  if (scm_is_pair (pair) && scm_is_number (scm_cdr (pair)))
    {
      *dest = scm_to_double (scm_cdr (pair));
      return true;
    }
  return false;
}

static SCM
add_stretchability (SCM alist, Real stretch)
{
  if (!scm_is_pair (scm_sloppy_assq (ly_symbol2scm ("stretchability"), alist)))
    return scm_acons (ly_symbol2scm ("stretchability"), scm_from_double (stretch), alist);

  return alist;
}

// We want to put a large stretch between a non-spaceable line and its
// non-affinity staff. We want to put an even larger stretch between
// a non-spaceable line and the top/bottom of the page. That way,
// a spacing-affinity UP line at the bottom of the page will still be
// placed close to its staff.
const double LARGE_STRETCH = 10e5;
const double HUGE_STRETCH = 10e7;

// Returns the spacing spec connecting BEFORE to AFTER.
SCM
Page_layout_problem::get_spacing_spec (Grob *before, Grob *after)
{
  // If there are no spacing wishes, return a very flexible spring.
  // This will occur, for example, if there are lyrics at the bottom of
  // the page, in which case we don't want the spring from the lyrics to
  // the bottom of the page to have much effect.
  if (!before || !after)
    return add_stretchability (SCM_EOL, HUGE_STRETCH);

  if (is_spaceable (before))
    {
      if (is_spaceable (after))
	return before->get_property ("next-staff-spacing");
      else
	{
	  Direction affinity = to_dir (after->get_property ("staff-affinity"));
	  return (affinity == DOWN)
	    ? add_stretchability (after->get_property ("non-affinity-spacing"), LARGE_STRETCH)
	    : after->get_property ("inter-staff-spacing");
	}
    }
  else
    {
      if (is_spaceable (after))
	{
	  Direction affinity = to_dir (before->get_property ("staff-affinity"));
	  return (affinity == UP)
	    ? add_stretchability (before->get_property ("non-affinity-spacing"), LARGE_STRETCH)
	    : before->get_property ("inter-staff-spacing");
	}
      else
	{
	  Direction before_affinity = to_dir (before->get_property ("staff-affinity"));
	  Direction after_affinity = to_dir (after->get_property ("staff-affinity"));
	  if (after_affinity > before_affinity)
	    {
	      warning (_ ("staff-affinities should only decrease"));
	      after_affinity = before_affinity;
	    }
	  if (before_affinity != UP)
	    return before->get_property ("inter-loose-line-spacing");
	  else if (after_affinity != DOWN)
	    return before->get_property ("inter-loose-line-spacing");
	  return add_stretchability (before->get_property ("non-affinity-spacing"), LARGE_STRETCH);
	}
    }

  assert (0);
  return SCM_BOOL_F;
}

void
Page_layout_problem::alter_spring_from_spacing_spec (SCM spec, Spring* spring)
{
  Real space;
  Real stretch;
  Real min_dist;
  if (read_spacing_spec (spec, &space, ly_symbol2scm ("space")))
    spring->set_distance (space);
  if (read_spacing_spec (spec, &min_dist, ly_symbol2scm ("minimum-distance")))
    spring->set_min_distance (min_dist);
  spring->set_default_strength ();

  if (read_spacing_spec (spec, &stretch, ly_symbol2scm ("stretchability")))
    {
      spring->set_inverse_stretch_strength (stretch);
      spring->set_inverse_compress_strength (stretch);
    }
}

vector<Grob*>
Page_layout_problem::filter_dead_elements (vector<Grob*> const& input)
{
  vector<Grob*> output;
  for (vsize i = 0; i < input.size (); ++i)
    {
      if (Hara_kiri_group_spanner::has_interface (input[i]))
	Hara_kiri_group_spanner::consider_suicide (input[i]);

      if (input[i]->is_live ())
	output.push_back (input[i]);
    }

  return output;
}
