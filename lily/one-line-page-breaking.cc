/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012--2022 Joe Neeman <joeneeman@gmail.com>

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

#include "one-line-page-breaking.hh"

#include "column-x-positions.hh"
#include "international.hh"
#include "output-def.hh"
#include "page-spacing.hh"
#include "paper-book.hh"
#include "paper-score.hh"
#include "simple-spacer.hh"
#include "system.hh"

#include <limits>

using std::vector;

One_line_page_breaking::One_line_page_breaking (Paper_book *pb)
  : Page_breaking (pb, 0, 0)
{
}

One_line_page_breaking::~One_line_page_breaking ()
{
}

/*
  This is a somewhat unconventional page-breaking algorithm.  Every
  score is put on a single page, whose width is enough to fit the entire
  score on one line.  Line breaks and page breaks are ignored, and the
  paper-width setting in the paper block is modified to fit the music.
*/
SCM
One_line_page_breaking::solve_and_provide_max_height (Real &max_height)
{
  Real max_width = 0;
  SCM all_pages = SCM_EOL;
  for (vsize i = 0; i < system_specs_.size (); ++i)
    {
      if (Paper_score *ps = system_specs_[i].pscore_)
        {
          vector<Paper_column *> cols = ps->root_system ()->used_columns ();

          // No indent, "infinite" line width, ragged.
          Column_x_positions pos = get_line_configuration (
            cols, std::numeric_limits<Real>::max (), 0, true);
          vector<Column_x_positions> positions;
          positions.push_back (pos);

          ps->root_system ()->break_into_pieces (positions);
          ps->root_system ()->do_break_substitution_and_fixup_refpoints ();
          Grob *system = ps->root_system ()->broken_intos_[0];

          vector<vsize> lines_per_page;
          lines_per_page.push_back (1);
          SCM systems = ly_list (system->self_scm ());
          SCM pages = make_pages (lines_per_page, systems);

          max_width
            = std::max (max_width, system->extent (system, X_AXIS).length ());
          max_height
            = std::max (max_height, system->extent (system, Y_AXIS).length ());
          all_pages = scm_cons (scm_car (pages), all_pages);
        }
      else if (Prob *pb = system_specs_[i].prob_)
        // Because we don't call Page_breaking::systems in this algorithm,
        // we need to manually unprotect the titles.
        pb->unprotect ();
    }

  // Alter paper-width so that it is large enough to fit every system.
  // TODO: it might be nice to allow different pages to have different widths.
  // This would need support in the backends (eg. framework-ps.scm).
  Real right_margin
    = from_scm<double> (book_->paper ()->c_variable ("right-margin"), 0.0);
  Real left_margin
    = from_scm<double> (book_->paper ()->c_variable ("left-margin"), 0.0);
  Real width = max_width + right_margin + left_margin;
  book_->paper ()->set_variable (ly_symbol2scm ("paper-width"), to_scm (width));

  return scm_reverse_x (all_pages, SCM_EOL);
}

SCM
One_line_page_breaking::solve ()
{
  Real unused_max_height = 0;
  return solve_and_provide_max_height (unused_max_height);
}
