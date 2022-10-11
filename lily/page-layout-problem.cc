/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2009--2022 Joe Neeman <joeneeman@gmail.com>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
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
#include "paper-score.hh"
#include "pointer-group-interface.hh"
#include "prob.hh"
#include "skyline-pair.hh"
#include "system.hh"
#include "text-interface.hh"
#include "lily-imports.hh"

using std::vector;

/*
 Returns the number of footnotes associated with a given line.
*/

vector<Grob *>
Page_layout_problem::get_footnote_grobs (SCM lines)
{
  vector<Grob *> footnotes;
  for (SCM s = lines; scm_is_pair (s); s = scm_cdr (s))
    {
      if (Grob *g = unsmob<Grob> (scm_car (s)))
        {
          System *sys = dynamic_cast<System *> (g);
          if (!sys)
            {
              programming_error (
                "got a grob for footnotes that wasn't a System");
              continue;
            }
          extract_grob_set (sys, "footnotes-after-line-breaking",
                            footnote_grobs);
          footnotes.insert (footnotes.end (), footnote_grobs.begin (),
                            footnote_grobs.end ());
        }
      else if (Prob *p = unsmob<Prob> (scm_car (s)))
        {
          SCM stencils = get_property (p, "footnotes");
          if (scm_is_null (stencils))
            continue;
          for (SCM st = stencils; scm_is_pair (st); st = scm_cdr (st))
            footnotes.push_back (0);
        }
    }

  return footnotes;
}

vsize
Page_layout_problem::get_footnote_count (SCM lines)
{
  vector<Grob *> notes = get_footnote_grobs (lines);
  return notes.size ();
}

SCM
Page_layout_problem::get_footnotes_from_lines (SCM lines)
{
  if (!scm_is_pair (lines))
    return SCM_EOL;

  bool footnotes_added;
  if (Grob *g = unsmob<Grob> (scm_car (lines)))
    footnotes_added = !scm_is_null (get_property (g, "footnote-stencil"));
  else if (Prob *p = unsmob<Prob> (scm_car (lines)))
    footnotes_added = !scm_is_null (get_property (p, "footnote-stencil"));
  else
    {
      programming_error ("Systems on a page must be a prob or grob.");
      return SCM_EOL;
    }
  if (!footnotes_added)
    {
      programming_error (
        "Footnotes must be added to lines before they are retrieved.");
      return SCM_EOL;
    }

  SCM out = SCM_EOL;
  for (SCM s = lines; scm_is_pair (s); s = scm_cdr (s))
    {
      if (Grob *g = unsmob<Grob> (scm_car (s)))
        out = scm_cons (get_property (g, "footnote-stencil"), out);
      else if (Prob *p = unsmob<Prob> (scm_car (s)))
        out = scm_cons (get_property (p, "footnote-stencil"), out);
      else
        programming_error ("Systems on a page must be a prob or grob.");
    }

  return scm_reverse_x (out, SCM_EOL);
}

/*
   Adds a footnote stencil to each system.  This stencil may
   itself be comprised of several footnotes.

   This is a long function, but it seems better to keep it intact rather than
   splitting it into parts.
*/

void
Page_layout_problem::add_footnotes_to_lines (SCM lines, vsize counter,
                                             Paper_book *pb)
{
  /*
    first, we have to see how many footnotes are on this page.
    we need to do this first so that we can line them up
  */

  Output_def *paper = pb->paper ();

  if (!paper)
    {
      programming_error (
        "Cannot get footnotes because there is no valid paper block.");
      return;
    }

  SCM number_footnote_table
    = pb->top_paper ()->c_variable ("number-footnote-table");
  if (!scm_is_pair (number_footnote_table))
    number_footnote_table = SCM_EOL;
  SCM numbering_function = paper->c_variable ("footnote-numbering-function");
  SCM layout = paper->self_scm ();
  SCM props = Lily::layout_extract_page_properties (layout);
  Real padding = from_scm<double> (paper->c_variable ("footnote-padding"), 0.0);
  Real number_raise
    = from_scm<double> (paper->c_variable ("footnote-number-raise"), 0.0);

  vector<Grob *> fn_grobs = get_footnote_grobs (lines);
  vsize fn_count = fn_grobs.size ();

  // now, make the footnote stencils with the numbering function
  SCM numbers = SCM_EOL;
  SCM in_text_numbers = SCM_EOL;
  /*
    TODO: This recalculates numbering every time this function is called, including once
    after the balloon prints are called.  Although it is not a huge computational drain,
    it'd be more elegant to turn this calculation off when it is no longer needed.

    In a separate commit, it'd be nice to streamline the way that page layout property
    is handled so that the process of building `config's in page-breaking does result
    in duplicated work, either by making this process less complicated or (preferably)
    by passing its results downstream.
  */

  // find the maximum X_AXIS length
  Real max_length = -infinity_f;

  for (vsize i = 0; i < fn_count; i++)
    {
      if (fn_grobs[i])
        {
          SCM assertion_function
            = get_property (fn_grobs[i], "numbering-assertion-function");
          if (ly_is_procedure (assertion_function))
            (void) ly_call (assertion_function, to_scm (counter));
        }
      SCM markup = ly_call (numbering_function, to_scm (counter));
      SCM stencil = Text_interface::interpret_markup (layout, props, markup);
      auto *st = unsmob<const Stencil> (stencil);
      if (!st)
        {
          programming_error (
            "Your numbering function needs to return a stencil.");
          markup = SCM_EOL;
          stencil = Stencil (Box (Interval (0, 0), Interval (0, 0)), SCM_EOL)
                      .smobbed_copy ();
          st = unsmob<const Stencil> (stencil);
        }
      in_text_numbers = scm_cons (markup, in_text_numbers);
      numbers = scm_cons (stencil, numbers);

      if (!st->extent (X_AXIS).is_empty ())
        max_length = std::max (max_length, st->extent (X_AXIS)[RIGHT]);

      counter++;
    }

  in_text_numbers = scm_reverse_x (in_text_numbers, SCM_EOL);

  // Reverse the list and translate each stencil such that it attains the
  // correct maximum length and bundle footnotes into a scheme object.
  {
    SCM reverse_numbers = numbers;
    numbers = SCM_EOL;
    for (SCM p = reverse_numbers; scm_is_pair (p); p = scm_cdr (p))
      {
        SCM st_scm = scm_car (p);
        auto *orig = unsmob<const Stencil> (st_scm);
        if (!orig->extent (X_AXIS).is_empty ())
          {
            auto trans = *orig;
            trans.translate_axis ((max_length - orig->extent (X_AXIS)[RIGHT]),
                                  X_AXIS);
            st_scm = trans.smobbed_copy ();
          }
        numbers = scm_cons (st_scm, numbers);
      }
  }

  // build the footnotes

  for (SCM s = lines; scm_is_pair (s); s = scm_cdr (s))
    {
      // Take care of musical systems.
      if (Grob *g = unsmob<Grob> (scm_car (s)))
        {
          System *sys = dynamic_cast<System *> (g);
          if (!sys)
            {
              programming_error (
                "got a grob for footnotes that wasn't a System");
              continue;
            }
          Stencil mol;
          Stencil in_note_mol;
          extract_grob_set (sys, "footnotes-after-line-breaking",
                            footnote_grobs);
          for (vsize i = 0; i < footnote_grobs.size (); i++)
            {
              Grob *footnote = footnote_grobs[i];
              SCM footnote_markup = get_property (footnote, "footnote-text");
              if (Spanner *orig = dynamic_cast<Spanner *> (footnote))
                if (orig->is_broken ())
                  footnote_markup
                    = get_property (orig->broken_intos_[0], "footnote-text");

              SCM props
                = Lily::layout_extract_page_properties (paper->self_scm ());

              auto footnote_stencil = Text_interface::interpret_markup (
                paper, props, footnote_markup);
              bool do_numbering = from_scm<bool> (
                get_property (footnote, "automatically-numbered"));
              if (Spanner *orig = dynamic_cast<Spanner *> (footnote))
                {
                  if (orig->is_broken ())
                    for (vsize i = 0; i < orig->broken_intos_.size (); i++)
                      do_numbering
                        = do_numbering
                          || from_scm<bool> (get_property (
                            orig->broken_intos_[i], "automatically-numbered"));
                }
              if (do_numbering)
                {
                  SCM annotation_scm = scm_car (in_text_numbers);
                  set_property (footnote, "text", annotation_scm);
                  if (Spanner *orig = dynamic_cast<Spanner *> (footnote))
                    {
                      set_property (orig, "text", annotation_scm);
                      if (orig->is_broken ())
                        for (vsize i = 0; i < orig->broken_intos_.size (); i++)
                          set_property (orig->broken_intos_[i], "text",
                                        annotation_scm);
                    }

                  auto annotation = *unsmob<const Stencil> (scm_car (numbers));
                  annotation.translate_axis (
                    (footnote_stencil.extent (Y_AXIS)[UP] + number_raise
                     - annotation.extent (Y_AXIS)[UP]),
                    Y_AXIS);
                  footnote_stencil.add_at_edge (X_AXIS, LEFT, annotation, 0.0);
                  numbers = scm_cdr (numbers);
                  in_text_numbers = scm_cdr (in_text_numbers);
                }
              if (!footnote_stencil.is_empty ())
                {
                  if (from_scm<bool> (get_property (footnote, "footnote")))
                    mol.add_at_edge (Y_AXIS, DOWN, footnote_stencil, padding);
                  else
                    in_note_mol.add_at_edge (Y_AXIS, DOWN, footnote_stencil,
                                             padding);
                }
            }
          set_property (sys, "in-note-stencil", in_note_mol.smobbed_copy ());
          set_property (sys, "footnote-stencil", mol.smobbed_copy ());
        }
      // Take care of top-level markups
      else if (Prob *p = unsmob<Prob> (scm_car (s)))
        {
          SCM stencils = get_property (p, "footnotes");
          Stencil mol;

          for (SCM st = stencils; scm_is_pair (st); st = scm_cdr (st))
            {
              auto footnote_stencil = *unsmob<const Stencil> (scm_caddar (st));
              bool do_numbering = from_scm<bool> (scm_cadar (st));
              SCM in_text_stencil = SCM_EOL;
              if (do_numbering)
                {
                  auto annotation = *unsmob<const Stencil> (scm_car (numbers));
                  SCM in_text_annotation = scm_car (in_text_numbers);
                  in_text_stencil = Text_interface::interpret_markup (
                    layout, props, in_text_annotation);
                  if (!unsmob<const Stencil> (in_text_stencil))
                    in_text_stencil = SCM_EOL;
                  annotation.translate_axis (
                    (footnote_stencil.extent (Y_AXIS)[UP] + number_raise
                     - annotation.extent (Y_AXIS)[UP]),
                    Y_AXIS);
                  footnote_stencil.add_at_edge (X_AXIS, LEFT, annotation, 0.0);
                  numbers = scm_cdr (numbers);
                  in_text_numbers = scm_cdr (in_text_numbers);
                }
              else
                {
                  in_text_stencil = Stencil ().smobbed_copy ();
                }
              number_footnote_table
                = scm_cons (scm_cons (scm_caar (st), in_text_stencil),
                            number_footnote_table);
              if (!footnote_stencil.is_empty ())
                mol.add_at_edge (Y_AXIS, DOWN, footnote_stencil, padding);
            }
          set_property (p, "footnote-stencil", mol.smobbed_copy ());
        }
    }

  // note that this line of code doesn't do anything if numbering isn't turned on
  pb->top_paper ()->set_variable (ly_symbol2scm ("number-footnote-table"),
                                  number_footnote_table);
}

Stencil
Page_layout_problem::get_footnote_separator_stencil (Output_def *paper)
{
  SCM props = Lily::layout_extract_page_properties (paper->self_scm ());

  SCM markup = paper->c_variable ("footnote-separator-markup");

  if (!Text_interface::is_markup (markup))
    return Stencil ();

  return Text_interface::interpret_markup (paper, props, markup);
}

Stencil
Page_layout_problem::add_footnotes_to_footer (SCM footnotes, Stencil foot,
                                              Paper_book *pb)
{

  bool footnotes_found = false;
  Real footnote_padding
    = from_scm<double> (pb->paper ()->c_variable ("footnote-padding"), 0.0);
  Real footnote_footer_padding = from_scm<double> (
    pb->paper ()->c_variable ("footnote-footer-padding"), 0.0);

  footnotes = scm_reverse (footnotes);

  for (SCM s = footnotes; scm_is_pair (s); s = scm_cdr (s))
    {
      auto *stencil = unsmob<const Stencil> (scm_car (s));

      if (!stencil)
        continue;

      if (!stencil->is_empty ())
        {
          foot.add_at_edge (
            Y_AXIS, UP, *stencil,
            (!footnotes_found ? footnote_footer_padding : footnote_padding));
          footnotes_found = true;
        }
    }

  if (footnotes_found)
    {
      Stencil separator = get_footnote_separator_stencil (pb->paper ());
      if (!separator.is_empty ())
        foot.add_at_edge (Y_AXIS, UP, separator, footnote_padding);
    }

  return foot;
}

Page_layout_problem::Page_layout_problem (Paper_book *pb, SCM page_scm,
                                          SCM systems)
  : bottom_skyline_ (DOWN)
{
  Prob *page = unsmob<Prob> (page_scm);
  bottom_loose_baseline_ = 0;
  header_height_ = 0;
  footer_height_ = 0;
  header_padding_ = 0;
  footer_padding_ = 0;
  page_height_ = 100;
  force_ = 0;

  if (page)
    {
      auto *head = unsmob<const Stencil> (get_property (page, "head-stencil"));
      auto *foot = unsmob<const Stencil> (get_property (page, "foot-stencil"));

      Stencil foot_stencil = foot ? *foot : Stencil ();

      if (pb && pb->paper ())
        {
          SCM footnotes = get_footnotes_from_lines (systems);
          foot_stencil = add_footnotes_to_footer (footnotes, foot_stencil, pb);
        }
      else
        warning (_ ("A page layout problem has been initiated that cannot "
                    "accommodate footnotes."));

      header_height_ = head ? head->extent (Y_AXIS).length () : 0;
      footer_height_ = foot_stencil.extent (Y_AXIS).length ();
      page_height_
        = from_scm<double> (get_property (page, "paper-height"), 100);
    }

  // Initially, bottom_skyline_ represents the top of the page. Make
  // it solid, so that the top of the first system will be forced
  // below the top of the printable area.
  bottom_skyline_.set_minimum_height (-header_height_);

  SCM system_system_spacing = SCM_EOL;
  SCM score_system_spacing = SCM_EOL;
  SCM markup_system_spacing = SCM_EOL;
  SCM score_markup_spacing = SCM_EOL;
  SCM markup_markup_spacing = SCM_EOL;

  // top_system_spacing controls the spring from the top of the printable
  // area to the first staff. It allows the user to control the offset of
  // the first staff (as opposed to the top of the first system) from the
  // top of the page. Similarly for last_bottom_spacing.
  SCM top_system_spacing = SCM_EOL;
  SCM last_bottom_spacing = SCM_EOL;
  if (pb && pb->paper ())
    {
      Output_def *paper = pb->paper ();
      system_system_spacing = paper->c_variable ("system-system-spacing");
      score_system_spacing = paper->c_variable ("score-system-spacing");
      markup_system_spacing = paper->c_variable ("markup-system-spacing");
      score_markup_spacing = paper->c_variable ("score-markup-spacing");
      markup_markup_spacing = paper->c_variable ("markup-markup-spacing");
      last_bottom_spacing = paper->c_variable ("last-bottom-spacing");
      top_system_spacing = paper->c_variable ("top-system-spacing");
      if (scm_is_pair (systems) && unsmob<Prob> (scm_car (systems)))
        top_system_spacing = paper->c_variable ("top-markup-spacing");

      // Note: the page height here does _not_ reserve space for headers and
      // footers. This is because we want to anchor the top-system-spacing
      // spring at the _top_ of the header.
      page_height_
        -= from_scm<double> (paper->c_variable ("top-margin"), 0)
           + from_scm<double> (paper->c_variable ("bottom-margin"), 0);

      read_spacing_spec (top_system_spacing, &header_padding_,
                         ly_symbol2scm ("padding"));
      read_spacing_spec (last_bottom_spacing, &footer_padding_,
                         ly_symbol2scm ("padding"));
      in_note_padding_
        = from_scm<double> (paper->c_variable ("in-note-padding"), 0.5);
      in_note_direction_
        = from_scm (paper->c_variable ("in-note-direction"), UP);
    }
  bool last_system_was_title = false;

  for (SCM s = systems; scm_is_pair (s); s = scm_cdr (s))
    {
      bool first = scm_is_eq (s, systems);

      if (Grob *g = unsmob<Grob> (scm_car (s)))
        {
          System *sys = dynamic_cast<System *> (g);
          if (!sys)
            {
              programming_error (
                "got a grob for vertical spacing that wasn't a System");
              continue;
            }

          SCM spec = system_system_spacing;
          if (first)
            spec = top_system_spacing;
          else if (last_system_was_title)
            spec = markup_system_spacing;
          else if (0 == sys->get_bound (LEFT)->get_rank ())
            spec = score_system_spacing;

          Spring spring (0, 0);
          Real padding = 0.0;
          Real indent = line_dimension_interval (sys->paper_score ()->layout (),
                                                 sys->get_rank ())[LEFT];
          alter_spring_from_spacing_spec (spec, &spring);
          read_spacing_spec (spec, &padding, ly_symbol2scm ("padding"));

          append_system (sys, spring, indent, padding);
          last_system_was_title = false;
        }
      else if (Prob *p = unsmob<Prob> (scm_car (s)))
        {
          SCM spec = first ? top_system_spacing
                           : (last_system_was_title ? markup_markup_spacing
                                                    : score_markup_spacing);
          Spring spring (0, 0);
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
  alter_spring_from_spacing_spec (last_bottom_spacing, &last_spring);
  read_spacing_spec (last_bottom_spacing, &last_padding,
                     ly_symbol2scm ("padding"));
  last_spring.ensure_min_distance (last_padding - bottom_skyline_.max_height ()
                                   + footer_height_);
  springs_.push_back (last_spring);

  if (elements_.size ())
    {
      Real bottom_padding = 0;

      // TODO: junk bottom-space now that we have last-bottom-spacing?
      // bottom-space has the flexibility that one can do it per-system.
      // NOTE: bottom-space is misnamed since it is not stretchable space.
      if (Prob *p = elements_.back ().prob)
        bottom_padding = from_scm<double> (get_property (p, "bottom-space"), 0);
      else if (elements_.back ().staves.size ())
        {
          SCM details = get_details (elements_.back ());
          bottom_padding = from_scm<double> (
            ly_assoc_get (ly_symbol2scm ("bottom-space"), details, SCM_BOOL_F),
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
Page_layout_problem::append_system (System *sys, Spring const &spring,
                                    Real indent, Real padding)
{
  Grob *align = unsmob<Grob> (get_object (sys, "vertical-alignment"));
  if (!align)
    return;

  set_property (align, "positioning-done", SCM_BOOL_T);

  extract_grob_set (align, "elements", all_elts);
  vector<Grob *> elts = filter_dead_elements (all_elts);
  vector<Real> minimum_offsets
    = Align_interface::get_minimum_translations_without_min_dist (align, elts,
                                                                  Y_AXIS);
  vector<Real> minimum_offsets_with_min_dist
    = Align_interface::get_minimum_translations (align, elts, Y_AXIS);

  Skyline up_skyline (UP);
  Skyline down_skyline (DOWN);
  build_system_skyline (elts, minimum_offsets_with_min_dist, &up_skyline,
                        &down_skyline);
  up_skyline.shift (indent);
  down_skyline.shift (indent);
  auto *in_note_stencil
    = unsmob<const Stencil> (get_property (sys, "in-note-stencil"));

  if (in_note_stencil && in_note_stencil->extent (Y_AXIS).length () > 0)
    {
      set_property (sys, "in-note-padding", to_scm (in_note_padding_));
      set_property (sys, "in-note-direction", to_scm (in_note_direction_));
      Skyline *sky = in_note_direction_ == UP ? &up_skyline : &down_skyline;
      sky->set_minimum_height (
        sky->max_height ()
        + in_note_direction_
            * (in_note_padding_ + in_note_stencil->extent (Y_AXIS).length ()));
    }

  /*
    We need to call distance with skyline-horizontal-padding because
    the system skyline-horizontal-padding is not added during the creation
    of an individual staff.  So we add the padding for the distance check
    at the time of adding in the system.
  */
  Real minimum_distance
    = up_skyline.distance (
        bottom_skyline_,
        from_scm<double> (get_property (sys, "skyline-horizontal-padding"), 0))
      + padding;

  Spring spring_copy = spring;
  spring_copy.ensure_min_distance (minimum_distance);
  springs_.push_back (spring_copy);

  if (elts.size () && !is_spaceable (elts[0]))
    {
      // store the minimum distance, considering relative indents,
      // for a loose line
      Skyline first_skyline (UP);
      SCM sky_scm = get_property (elts[0], "vertical-skylines");
      const Skyline_pair &sky = from_scm<Skyline_pair> (sky_scm);
      first_skyline.merge (sky[UP]);
      first_skyline.shift (indent);
      minimum_distance
        = first_skyline.distance (bottom_skyline_) - bottom_loose_baseline_;
    }
  bottom_skyline_ = down_skyline;
  elements_.push_back (
    Element (elts, minimum_offsets, minimum_distance, padding));

  // Add the springs for the VerticalAxisGroups in this system.

  // If the user has specified the offsets of the individual staves, fix the
  // springs at the given distances. Otherwise, use stretchable springs.
  SCM details = get_details (elements_.back ());
  SCM manual_dists
    = ly_assoc_get (ly_symbol2scm ("alignment-distances"), details, SCM_EOL);
  vsize last_spaceable_staff = 0;
  bool found_spaceable_staff = false;
  for (vsize i = 0; i < elts.size (); ++i)
    {
      if (is_spaceable (elts[i]))
        {
          if (!found_spaceable_staff)
            {
              // Ensure space for any loose lines above this system
              if (i > 0)
                springs_.back ().ensure_min_distance (
                  bottom_loose_baseline_ - minimum_offsets_with_min_dist[i]
                  + padding);
              found_spaceable_staff = true;
              last_spaceable_staff = i;
              // We don't add a spring for the first staff, since
              // we are only adding springs _between_ staves here.
              continue;
            }

          Spring spring (0.5, 0.0);
          SCM spec
            = get_property (elts[last_spaceable_staff], "staff-staff-spacing");
          // In the event an override specifies
          // staff-staff-spacing.some-property spec will be a non-list pair with
          // an unpure-pure container as cdr. Remove it before continuing.
          if (!ly_is_list (spec))
            // spec needs to be an alist
            spec = scm_cons (scm_car (spec), SCM_EOL);

          // Substitute default values for any that are missing
          SCM default_spacing = get_property (elts[last_spaceable_staff],
                                              "default-staff-staff-spacing");
          if (scm_is_pair (default_spacing) && ly_is_list (spec))
            for (SCM s = default_spacing; scm_is_pair (s); s = scm_cdr (s))
              if (!scm_is_pair (scm_assq (scm_caar (s), spec)))
                spec = scm_cons (scm_car (s), spec);

          alter_spring_from_spacing_spec (spec, &spring);

          springs_.push_back (spring);
          Real min_distance
            = (found_spaceable_staff
                 ? minimum_offsets_with_min_dist[last_spaceable_staff]
                 : 0)
              - minimum_offsets_with_min_dist[i];
          springs_.back ().ensure_min_distance (min_distance);

          if (scm_is_pair (manual_dists))
            {
              if (scm_is_number (scm_car (manual_dists)))
                {
                  Real dy = from_scm<double> (scm_car (manual_dists));

                  springs_.back ().set_ideal_distance (dy);
                  springs_.back ().set_min_distance (dy);
                  springs_.back ().set_inverse_stretch_strength (0);
                }
              manual_dists = scm_cdr (manual_dists);
            }
          last_spaceable_staff = i;
        }
    }

  bottom_loose_baseline_
    = found_spaceable_staff
        ? (minimum_offsets_with_min_dist[last_spaceable_staff]
           - minimum_offsets_with_min_dist.back ())
        : 0;

  // Corner case: there was only one staff, and it wasn't spaceable.
  if (!found_spaceable_staff && elts.size ())
    mark_as_spaceable (elts[0]);
}

void
Page_layout_problem::append_prob (Prob *prob, Spring const &spring,
                                  Real padding)
{
  SCM sky_scm = get_property (prob, "vertical-skylines");
  Real minimum_distance = 0;
  bool tight_spacing = from_scm<bool> (get_property (prob, "tight-spacing"));

  if (is_scm<Skyline_pair> (sky_scm))
    {
      const Skyline_pair &sky = from_scm<Skyline_pair> (sky_scm);
      minimum_distance
        = std::max (sky[UP].distance (bottom_skyline_), bottom_loose_baseline_);
      bottom_skyline_ = sky[DOWN];
    }
  else if (auto *sten = unsmob<const Stencil> (get_property (prob, "stencil")))
    {
      Interval iv = sten->extent (Y_AXIS);
      minimum_distance = iv[UP] - bottom_skyline_.max_height ();

      bottom_skyline_.clear ();
      bottom_skyline_.set_minimum_height (iv[DOWN]);
    }
  bottom_loose_baseline_ = 0.0;

  Spring spring_copy = spring;
  if (tight_spacing)
    {
      spring_copy.set_min_distance (minimum_distance);
      spring_copy.set_inverse_stretch_strength (0.0);
      spring_copy.set_ideal_distance (0.0);
    }
  else
    spring_copy.ensure_min_distance (minimum_distance + padding);

  springs_.push_back (spring_copy);
  elements_.push_back (Element (prob, padding));
}

/**
   For ragged-last pages, we usually want to stretch the page so that it
   is not much more compressed than the previous page.  Here, if ragged is
   true and you pass a value of fixed_force that !isinf, then I will try
   to space this page using the given force.  If it does not fit, I will
   resort to just filling the page (non-raggedly).
*/
void
Page_layout_problem::solve_rod_spring_problem (bool ragged, Real fixed_force)
{
  Simple_spacer spacer;

  for (vsize i = 0; i < springs_.size (); ++i)
    spacer.add_spring (springs_[i]);

  Simple_spacer::Solution sol;
  if (ragged && !std::isinf (fixed_force))
    {
      // We need to tell the spacer it isn't ragged.  Otherwise, it will
      // refuse to stretch.
      sol = spacer.solve (page_height_, false);

      if (spacer.configuration_length (fixed_force) <= page_height_)
        sol.force_ = fixed_force;

      solution_ = spacer.spring_positions (sol.force_, false);
    }
  else
    {
      sol = spacer.solve (page_height_, ragged);
      force_ = sol.force_;
      solution_ = spacer.spring_positions (sol.force_, ragged);
    }

  if (!sol.fits_)
    {
      Real overflow = spacer.configuration_length (sol.force_) - page_height_;
      if (ragged && overflow < 1e-6)
        warning (
          _ ("ragged-bottom was specified, but page must be compressed"));
      else
        {
          warning (
            _f ("compressing over-full page by %.1f staff-spaces", overflow));
          force_ = -infinity_f;
          vsize space_count = solution_.size ();
          Real spacing_increment
            = overflow / static_cast<Real> (space_count - 2);
          for (vsize i = 2; i < space_count; i++)
            solution_[i] -= static_cast<Real> (i - 1) * spacing_increment;
        }
    }
}

Real
Page_layout_problem::force () const
{
  return force_;
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
  vector<Grob *> loose_lines;
  vector<Real> loose_line_min_distances;
  Grob *last_spaceable_line = 0;
  Real last_spaceable_line_translation = 0;
  Interval last_title_extent;
  for (vsize i = 0; i < elements_.size (); ++i)
    {
      if (elements_[i].prob)
        {
          *tail = scm_cons (to_scm (solution_[spring_idx]), SCM_EOL);
          tail = SCM_CDRLOC (*tail);
          Interval prob_extent = unsmob<const Stencil> (
                                   get_property (elements_[i].prob, "stencil"))
                                   ->extent (Y_AXIS);

          // Lay out any non-spaceable lines between this line and
          // the last one.
          if (loose_lines.size ())
            {
              Interval loose_extent
                = loose_lines.back ()->extent (loose_lines.back (), Y_AXIS);
              Real min_distance = (-loose_extent[DOWN] + prob_extent[UP]
                                   + elements_[i].padding);

              loose_line_min_distances.push_back (min_distance);
              loose_lines.push_back (0);

              distribute_loose_lines (loose_lines, loose_line_min_distances,
                                      last_spaceable_line_translation,
                                      -solution_[spring_idx]);
              loose_lines.clear ();
              loose_line_min_distances.clear ();
            }

          last_spaceable_line = 0;
          last_spaceable_line_translation = -solution_[spring_idx];
          last_title_extent = prob_extent;
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
          Real first_staff_min_translation = elements_[i].min_offsets.size ()
                                               ? elements_[i].min_offsets[0]
                                               : 0;
          Real system_position
            = first_staff_position + first_staff_min_translation;

          // Position the staves within this system.
          vector<Real> const &min_offsets = elements_[i].min_offsets;
          bool found_spaceable_staff = false;
          for (vsize staff_idx = 0; staff_idx < elements_[i].staves.size ();
               ++staff_idx)
            {
              Grob *staff = elements_[i].staves[staff_idx];
              set_property (staff, "system-Y-offset",
                            to_scm (-system_position));

              if (is_spaceable (staff))
                {
                  // this is relative to the system: negative numbers are down.
                  staff->translate_axis (
                    system_position - solution_[spring_idx], Y_AXIS);

                  // Lay out any non-spaceable lines between this line and
                  // the last one.
                  if (loose_lines.size ())
                    {
                      if (staff_idx)
                        loose_line_min_distances.push_back (
                          min_offsets[staff_idx - 1] - min_offsets[staff_idx]);
                      else
                        {
                          // A null line to break any staff-affinity from the previous system
                          loose_line_min_distances.push_back (0.0);
                          loose_lines.push_back (0);
                          loose_line_min_distances.push_back (
                            elements_[i].padding - min_offsets[0]);
                        }
                      loose_lines.push_back (staff);

                      distribute_loose_lines (loose_lines,
                                              loose_line_min_distances,
                                              last_spaceable_line_translation,
                                              -solution_[spring_idx]);
                      loose_lines.clear ();
                      loose_line_min_distances.clear ();
                    }
                  last_spaceable_line = staff;
                  last_spaceable_line_translation = -solution_[spring_idx];
                  found_spaceable_staff = true;
                  spring_idx++;
                }
              else // ! is_spaceable
                {
                  if (staff->extent (staff, Y_AXIS).is_empty ())
                    continue;

                  if (loose_lines.empty ())
                    loose_lines.push_back (last_spaceable_line);

                  if (staff_idx)
                    // NOTE: the way we do distances between loose lines (and other lines too, actually)
                    // is not the most accurate way possible: we only insert rods between adjacent
                    // lines.  To be more accurate, we could insert rods between non-adjacent lines
                    // using a scheme similar to the one in set_column_rods.
                    loose_line_min_distances.push_back (
                      min_offsets[staff_idx - 1] - min_offsets[staff_idx]);
                  else
                    {
                      // this is the first line in a system
                      Real min_dist = 0;
                      if (loose_lines.back ())
                        {
                          // distance to the final line in the preceding system,
                          // including 'system-system-spacing 'padding
                          min_dist
                            = elements_[i].min_distance + elements_[i].padding;
                          // A null line to break any staff-affinity for the previous system
                          loose_line_min_distances.push_back (0.0);
                          loose_lines.push_back (0);
                        }
                      else if (!last_title_extent.is_empty ())
                        // distance to the preceding title,
                        //  including 'markup-system-wg 'padding
                        min_dist
                          = (staff->extent (staff, Y_AXIS)[UP]
                             - last_title_extent[DOWN] + elements_[i].padding);
                      else // distance to the top margin
                        min_dist = header_padding_ + header_height_
                                   + staff->extent (staff, Y_AXIS)[UP];

                      loose_line_min_distances.push_back (min_dist);
                    }
                  loose_lines.push_back (staff);
                }
            }

          // Corner case: even if a system has no live staves, it still takes up
          // one spring (a system with one live staff also takes up one spring),
          // which we need to increment past.
          if (!found_spaceable_staff)
            spring_idx++;

          *tail = scm_cons (to_scm (system_position), SCM_EOL);
          tail = SCM_CDRLOC (*tail);
        }
    }

  if (loose_lines.size ())
    {
      Grob *last = loose_lines.back ();
      Interval last_ext = last->extent (last, Y_AXIS);
      loose_line_min_distances.push_back (-last_ext[DOWN] + footer_height_
                                          + footer_padding_);
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
// first_translation and last_translation are relative to the page.
void
Page_layout_problem::distribute_loose_lines (vector<Grob *> const &loose_lines,
                                             vector<Real> const &min_distances,
                                             Real first_translation,
                                             Real last_translation)
{
  Simple_spacer spacer;
  for (vsize i = 0; i + 1 < loose_lines.size (); ++i)
    {
      SCM spec = get_spacing_spec (loose_lines[i], loose_lines[i + 1], false, 0,
                                   INT_MAX);
      Spring spring (1.0, 0.0);
      alter_spring_from_spacing_spec (spec, &spring);
      spring.ensure_min_distance (min_distances[i]);
      spacer.add_spring (spring);
    }

  // Remember: offsets are decreasing, since we're going from UP to DOWN!
  Simple_spacer::Solution sol
    = spacer.solve (first_translation - last_translation, false);

  vector<Real> solution = spacer.spring_positions (sol.force_, false);
  for (vsize i = 1; i + 1 < solution.size (); ++i)
    if (loose_lines[i])
      {
        Real system_offset
          = from_scm<double> (get_property (loose_lines[i], "system-Y-offset"));
        loose_lines[i]->translate_axis (
          first_translation - solution[i] - system_offset, Y_AXIS);
      }
}

SCM
Page_layout_problem::fixed_force_solution (Real force)
{
  solve_rod_spring_problem (true, force);
  return find_system_offsets ();
}

SCM
Page_layout_problem::solution (bool ragged)
{
  solve_rod_spring_problem (ragged, -infinity_f);
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
Page_layout_problem::build_system_skyline (
  vector<Grob *> const &staves, vector<Real> const &minimum_translations,
  Skyline *up, Skyline *down)
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
      SCM sky_scm = get_property (g, "vertical-skylines");
      if (is_scm<Skyline_pair> (sky_scm))
        {
          const Skyline_pair &sky = from_scm<Skyline_pair> (sky_scm);
          up->raise (-dy);
          up->merge (sky[UP]);
          up->raise (dy);

          down->raise (-dy);
          down->merge (sky[DOWN]);
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
  auto *sten = unsmob<const Stencil> (get_property (p, "stencil"));
  return sten ? sten->extent (Y_AXIS) : Interval (0, 0);
}

Interval
Page_layout_problem::first_staff_extent (Element const &e)
{
  if (e.prob)
    return prob_extent (e.prob);
  else if (e.staves.size ())
    return e.staves[0]->extent (e.staves[0], Y_AXIS);

  return Interval (0, 0);
}

Interval
Page_layout_problem::last_staff_extent (Element const &e)
{
  if (e.prob)
    return prob_extent (e.prob);
  else if (e.staves.size ())
    return e.staves.back ()->extent (e.staves.back (), Y_AXIS);

  return Interval (0, 0);
}

SCM
Page_layout_problem::get_details (Element const &elt)
{
  if (elt.staves.empty ())
    return SCM_EOL;

  return get_details (elt.staves.back ()->get_system ());
}

SCM
Page_layout_problem::get_details (Spanner *sp)
{
  return get_property (sp->get_bound (LEFT), "line-break-system-details");
}

bool
Page_layout_problem::is_spaceable (Grob *g)
{
  return !scm_is_number (get_property (g, "staff-affinity"));
}

void
Page_layout_problem::mark_as_spaceable (Grob *g)
{
  set_property (g, "staff-affinity", SCM_BOOL_F);
}

bool
Page_layout_problem::read_spacing_spec (SCM spec, Real *dest, SCM sym)
{
  SCM pair = scm_sloppy_assq (sym, spec);
  if (scm_is_pair (pair) && scm_is_number (scm_cdr (pair)))
    {
      *dest = from_scm<double> (scm_cdr (pair));
      return true;
    }
  return false;
}

// If there is a forced, fixed spacing between BEFORE and AFTER, return it.
// Otherwise, return -infinity_f.
// If after is spaceable, it is the (spaceable_index + 1)th spaceable grob in
// its alignment.
Real
Page_layout_problem::get_fixed_spacing (Grob *before, Grob *after,
                                        int spaceable_index, bool pure,
                                        vsize start, vsize end)
{
  Spanner *after_sp = dynamic_cast<Spanner *> (after);
  SCM cache_symbol = (is_spaceable (before) && is_spaceable (after))
                       ? ly_symbol2scm ("spaceable-fixed-spacing")
                       : ly_symbol2scm ("loose-fixed-spacing");
  if (pure)
    {
      // The result of this function doesn't depend on "end," so we can reduce the
      // size of the cache by ignoring it.
      SCM cached = after_sp->get_cached_pure_property (cache_symbol, start, 0);
      if (scm_is_number (cached))
        return from_scm<double> (cached, 0.0);
    }

  Real ret = -infinity_f;

  // If we're pure, then paper-columns have not had their systems set,
  // and so elts[i]->get_system () is unreliable.
  System *sys = pure ? Grob::get_system (before) : before->get_system ();
  Grob *left_bound
    = sys ? sys->get_maybe_pure_bound (LEFT, pure, start, end) : 0;

  if (is_spaceable (before) && is_spaceable (after) && left_bound)
    {
      SCM details = get_property (left_bound, "line-break-system-details");
      SCM manual_dists = ly_assoc_get (ly_symbol2scm ("alignment-distances"),
                                       details, SCM_EOL);
      if (scm_is_pair (manual_dists))
        {
          SCM forced = robust_list_ref (spaceable_index - 1, manual_dists);
          if (scm_is_number (forced))
            ret = std::max (ret, from_scm<double> (forced));
        }
    }

  // Cache the result.  As above, we ignore "end."
  if (pure)
    after_sp->cache_pure_property (cache_symbol, start, 0, to_scm (ret));

  return ret;
}

static SCM
add_stretchability (SCM alist, Real stretch)
{
  if (!scm_is_pair (scm_sloppy_assq (ly_symbol2scm ("stretchability"), alist)))
    return scm_acons (ly_symbol2scm ("stretchability"), to_scm (stretch),
                      alist);

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
Page_layout_problem::get_spacing_spec (Grob *before, Grob *after, bool pure,
                                       vsize start, vsize end)
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
        return get_maybe_pure_property (before, "staff-staff-spacing", pure,
                                        start, end);
      else
        {
          Direction affinity = from_scm<Direction> (get_maybe_pure_property (
            after, "staff-affinity", pure, start, end));
          return (affinity == DOWN)
                   ? add_stretchability (
                     get_maybe_pure_property (after,
                                              "nonstaff-unrelatedstaff-spacing",
                                              pure, start, end),
                     LARGE_STRETCH)
                   : get_maybe_pure_property (
                     after, "nonstaff-relatedstaff-spacing", pure, start, end);
        }
    }
  else
    {
      if (is_spaceable (after))
        {
          Direction affinity = from_scm<Direction> (get_maybe_pure_property (
            before, "staff-affinity", pure, start, end));
          return (affinity == UP)
                   ? add_stretchability (
                     get_maybe_pure_property (before,
                                              "nonstaff-unrelatedstaff-spacing",
                                              pure, start, end),
                     LARGE_STRETCH)
                   : get_maybe_pure_property (
                     before, "nonstaff-relatedstaff-spacing", pure, start, end);
        }
      else
        {
          Direction before_affinity
            = from_scm<Direction> (get_maybe_pure_property (
              before, "staff-affinity", pure, start, end));
          Direction after_affinity
            = from_scm<Direction> (get_maybe_pure_property (
              after, "staff-affinity", pure, start, end));
          static bool warned = false;
          if (after_affinity > before_affinity && !warned && !pure)
            {
              warning (_ ("staff-affinities should only decrease"));
              warned = true;
            }
          if (before_affinity != UP)
            return get_maybe_pure_property (before, "nonstaff-nonstaff-spacing",
                                            pure, start, end);
          else if (after_affinity != DOWN)
            return get_maybe_pure_property (before, "nonstaff-nonstaff-spacing",
                                            pure, start, end);
          return add_stretchability (
            get_maybe_pure_property (before, "nonstaff-unrelatedstaff-spacing",
                                     pure, start, end),
            LARGE_STRETCH);
        }
    }

  assert (0);
  return SCM_BOOL_F;
}

void
Page_layout_problem::alter_spring_from_spacing_spec (SCM spec, Spring *spring)
{
  Real space;
  Real stretch;
  Real min_dist;
  if (read_spacing_spec (spec, &space, ly_symbol2scm ("basic-distance")))
    spring->set_ideal_distance (space);
  if (read_spacing_spec (spec, &min_dist, ly_symbol2scm ("minimum-distance")))
    spring->set_min_distance (min_dist);
  spring->set_default_strength ();

  if (read_spacing_spec (spec, &stretch, ly_symbol2scm ("stretchability")))
    spring->set_inverse_stretch_strength (stretch);
}

vector<Grob *>
Page_layout_problem::filter_dead_elements (vector<Grob *> const &input)
{
  vector<Grob *> output;
  for (vsize i = 0; i < input.size (); ++i)
    {
      if (has_interface<Hara_kiri_group_spanner> (input[i]))
        Hara_kiri_group_spanner::consider_suicide (input[i]);

      if (input[i]->is_live ())
        output.push_back (input[i]);
    }

  return output;
}
