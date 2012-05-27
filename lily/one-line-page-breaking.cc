/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012 Joe Neeman <joeneeman@gmail.com>

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

#include <limits>

#include "column-x-positions.hh"
#include "international.hh"
#include "output-def.hh"
#include "page-spacing.hh"
#include "paper-book.hh"
#include "paper-score.hh"
#include "simple-spacer.hh"
#include "system.hh"

One_line_page_breaking::One_line_page_breaking (Paper_book *pb)
  : Page_breaking (pb, 0, 0)
{
}

One_line_page_breaking::~One_line_page_breaking ()
{
}

/*
  This is a somewhat unconventional page-breaking algorithm. Every
  score will be put on a single page, whose width is enough
  to fit the entire score one one line. Line breaks and page breaks
  are ignored, as are the page dimensions in the paper block.
*/
SCM
One_line_page_breaking::solve ()
{
  SCM all_pages = SCM_EOL;
  for (vsize i = 0; i < system_specs_.size (); ++i)
    if (Paper_score *ps = system_specs_[i].pscore_)
      {
        vector<Grob*> cols = ps->root_system ()->used_columns ();

        // No indent, "infinite" line width, ragged.
        Column_x_positions pos = get_line_configuration (cols, numeric_limits<Real>::max (), 0, true);
        vector<Column_x_positions> positions;
        positions.push_back (pos);

        ps->root_system ()->break_into_pieces (positions);
        ps->root_system ()->do_break_substitution_and_fixup_refpoints ();
        Grob *system = ps->root_system ()->broken_intos_[0];
        Real width = system->extent (system, X_AXIS).length ();
        Real height = system->extent (system, Y_AXIS).length ();
        
        // HACK: probably shouldn't be modifying the paper_; better to modify the page
        // afterwards.
        book_->paper_->set_variable (ly_symbol2scm ("paper-width"), scm_from_double (width + 100));
        // TODO: figure out what the height should be.
        //book_->paper_->set_variable (ly_symbol2scm ("paper-height"), scm_from_double (height));
        vector<vsize> lines_per_page;
        lines_per_page.push_back (1);
        SCM systems = scm_list_1 (system->self_scm ());
        SCM pages = make_pages (lines_per_page, systems);
        all_pages = scm_cons (scm_car (pages), all_pages);
      }

  return scm_reverse_x (all_pages, SCM_EOL);
}
