/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2016--2022 Paul Morris

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

#include "international.hh"
#include "one-page-breaking.hh"
#include "output-def.hh"
#include "page-spacing.hh"
#include "paper-book.hh"
#include "paper-score.hh"
#include "system.hh"

using std::vector;

One_page_breaking::One_page_breaking (Paper_book *pb)
  : Page_breaking (pb, 0, 0)
{
}

One_page_breaking::~One_page_breaking ()
{
}

SCM
One_page_breaking::read_spacing_alist (SCM spec, SCM sym)
{
  SCM pair = scm_sloppy_assq (sym, spec);
  if (scm_is_pair (pair) && scm_is_number (scm_cdr (pair)))
    return scm_cdr (pair);
  else
    return to_scm (0);
}

/*
  This is a somewhat unconventional page-breaking algorithm.  The
  @code{paper-height} setting (in the paper block) is automatically
  modified to fit the height of the content so that everything exactly
  fits on a single page with no compression.  As usual (in the paper
  block) the width of the page can be customized with
  @code{paper-width} or @code{set-paper-size}, and the spacing between
  the footer and the last system (or top level markup) can be customized
  with @code{last-bottom-spacing}.

  It works by (1) temporarily setting the page height to a very large
  value, (2) doing line breaking and page breaking, much like in
  @code{ly:minimal-line-breaking}, (3) calculate and set the final
  height of the page based on the results, taking last-bottom-spacing,
  footer, top and bottom margins, etc. into account.
*/
SCM
One_page_breaking::solve ()
{
  // TEMPORARILY SET VERY LARGE PAPER HEIGHT
  // Stencil::translate throws a programming error (for the tagline
  // position) if this is set any larger than 1e6
  book_->paper ()->set_variable (ly_symbol2scm ("paper-height"), to_scm (1e6));

  // LINE BREAKING
  message (_ ("Calculating line breaks..."));
  vsize end = last_break_position ();
  set_to_ideal_line_configuration (0, end);
  break_into_pieces (0, end, current_configuration (0));

  // PAGE BREAKING
  message (_ ("Fitting music on 1 page..."));
  int first_page_num
    = from_scm (book_->paper ()->c_variable ("first-page-number"), 1);
  Page_spacing_result res = space_systems_on_n_pages (0, 1, first_page_num);
  SCM lines = systems ();
  SCM pages = make_pages (res.systems_per_page_, lines);

  // GET VERTICAL POSITIONS
  // Larger values are lower on the page.  We can't just use the last
  // one, because the last does not necessarily have the lowest bound.
  vector<Real> line_posns;
  SCM lowest_line_pos = to_scm (0);

  Prob *page_pb = unsmob<Prob> (scm_car (pages));
  SCM config = get_property (page_pb, "configuration");

  for (SCM c = config; scm_is_pair (c); c = scm_cdr (c))
    {
      SCM this_pos = scm_car (c);
      line_posns.push_back (from_scm<double> (this_pos));
      if (scm_is_true (scm_gr_p (this_pos, lowest_line_pos)))
        lowest_line_pos = this_pos;
    }

  // CALCULATE THE LOWEST LOWER BOUND OF ALL LINES ON THE PAGE
  vector<Real> line_heights;
  for (vsize i = 0; i < system_specs_.size (); i++)
    {
      if (Paper_score *ps = system_specs_[i].pscore_)
        {
          // musical systems
          vsize broken_intos_size = ps->root_system ()->broken_intos_.size ();
          for (vsize s = 0; s < broken_intos_size; s++)
            {
              Grob *system = ps->root_system ()->broken_intos_[s];
              line_heights.push_back (
                system->extent (system, Y_AXIS).length ());
            }
        }
      else if (Prob *pb = system_specs_[i].prob_)
        {
          // top-level markups
          auto *stil = unsmob<const Stencil> (get_property (pb, "stencil"));
          line_heights.push_back (stil->extent (Y_AXIS).length ());
        }
    }

  Real lowest_bound = 0;
  for (vsize i = 0; i < line_heights.size (); i++)
    {
      Real low_bound = line_heights[i] + line_posns[i];
      if (low_bound > lowest_bound)
        lowest_bound = low_bound;
    }

  // HANDLE LAST-BOTTOM-SPACING
  SCM last_bottom = book_->paper ()->c_variable ("last-bottom-spacing");

  SCM padding = read_spacing_alist (last_bottom, ly_symbol2scm ("padding"));
  lowest_bound += from_scm<double> (padding);

  SCM basic_dist
    = read_spacing_alist (last_bottom, ly_symbol2scm ("basic-distance"));
  SCM minimum_dist
    = read_spacing_alist (last_bottom, ly_symbol2scm ("minimum-distance"));
  SCM max_dist = scm_max (basic_dist, minimum_dist);

  // If the last line is a musical system get the distance between its
  // refpoint and its upper bound. If it is a top level markup its
  // refpoint is 0.
  SCM refpoint_dist = to_scm (0);

  SCM lines_probs = get_property (page_pb, "lines");
  Prob *last_line_pb = unsmob<Prob> (
    scm_list_ref (lines_probs, scm_oneminus (scm_length (lines_probs))));

  SCM refpoint_extent = get_property (last_line_pb, "staff-refpoint-extent");

  if (scm_is_pair (refpoint_extent)
      && scm_is_number (scm_car (refpoint_extent)))
    refpoint_dist = scm_product (scm_car (refpoint_extent), to_scm (-1));

  Real last_bottom_bound = from_scm<double> (
    scm_sum (lowest_line_pos, scm_sum (refpoint_dist, max_dist)));
  if (last_bottom_bound > lowest_bound)
    lowest_bound = last_bottom_bound;

  // SET FINAL PAPER HEIGHT
  auto *foot_stil
    = unsmob<const Stencil> (get_property (page_pb, "foot-stencil"));
  Real foot_height = foot_stil->extent (Y_AXIS).length ();

  SCM top_margin = book_->paper ()->c_variable ("top-margin");
  SCM bottom_margin = book_->paper ()->c_variable ("bottom-margin");
  SCM margins = scm_sum (top_margin, bottom_margin);

  SCM ppr_height = scm_sum (margins, to_scm (lowest_bound + foot_height));

  book_->paper ()->set_variable (ly_symbol2scm ("paper-height"), ppr_height);

  // bottom-edge determines placement of footer (tagline, footnotes, etc.)
  set_property (page_pb, "bottom-edge",
                scm_difference (ppr_height, bottom_margin));

  return pages;
}
