/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Joe Neeman <joeneeman@gmail.com>

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

#ifndef PAGE_TURN_PAGE_BREAKING_HH
#define PAGE_TURN_PAGE_BREAKING_HH

#include "page-breaking.hh"

#include <vector>

/*
  A dynamic programming solution to breaking pages
 */
class Page_turn_page_breaking : public Page_breaking
{
public:
  SCM solve () override;

  Page_turn_page_breaking (Paper_book *pb);
  virtual ~Page_turn_page_breaking ();

protected:
  struct Break_node
  {
    vsize prev_;
    int first_page_number_;
    vsize page_count_;

    /* true if every score here is too widely spaced */
    bool too_many_lines_;

    Real demerits_;
    vsize break_pos_; /* index into breaks_ */

    Line_division div_;
    std::vector<vsize> system_count_; /* systems per page */

    Break_node ()
    {
      prev_ = break_pos_ = VPOS;
      demerits_ = infinity_f;
      first_page_number_ = 0;
      page_count_ = 0;
      too_many_lines_ = false;
    }
  };

  std::vector<Break_node> state_;

  vsize total_page_count (Break_node const &b);
  Break_node put_systems_on_pages (vsize start, vsize end, vsize configuration,
                                   int page_number);

  SCM make_lines (std::vector<Break_node> *breaks);
  SCM make_pages (std::vector<Break_node> const &breaks, SCM systems);

  void calc_subproblem (vsize i);
  void print_break_node (Break_node const &b);
};
#endif /* PAGE_TURN_PAGE_BREAKING_HH */
