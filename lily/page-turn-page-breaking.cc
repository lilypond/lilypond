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

#include "page-turn-page-breaking.hh"

#include "international.hh"
#include "item.hh"
#include "output-def.hh"
#include "page-spacing.hh"
#include "paper-book.hh"
#include "paper-score.hh"
#include "paper-system.hh"
#include "system.hh"
#include "warn.hh"

#include <algorithm>
#include <vector>

using std::string;
using std::vector;

template <typename T>
static bool
is_break (T *g)
{
  bool turnable = scm_is_symbol (get_property (g, "page-turn-permission"));

  if (turnable)
    {
      bool page_breakable
        = scm_is_symbol (get_property (g, "page-break-permission"));
      bool line_breakable
        = scm_is_symbol (get_property (g, "line-break-permission"));
      if (!page_breakable || !line_breakable)
        {
          programming_error (
            "found a page-turnable place which was not breakable");
          turnable = false;
        }
    }

  return turnable;
}

Page_turn_page_breaking::Page_turn_page_breaking (Paper_book *pb)
  : Page_breaking (pb, is_break<Grob>, is_break<Prob>)
{
}

Page_turn_page_breaking::~Page_turn_page_breaking ()
{
}

/* Space the systems between start and end on a page, and calculate
   aggregate demerits taking demerits of break at start with the
   config of putting [start..end] on a page */
Page_turn_page_breaking::Break_node
Page_turn_page_breaking::put_systems_on_pages (vsize start, vsize end,
                                               vsize configuration,
                                               int page_number)
{
  vsize min_p_count = min_page_count (configuration, page_number);
  bool auto_first
    = from_scm<bool> (book_->paper ()->c_variable ("auto-first-page-number"));

  /* If [START, END] does not contain an intermediate
     breakpoint, we may need to consider solutions that result in a bad turn.
     In this case, we won't abort if the min_page_count is too big */
  if (start < end - 1 && min_p_count + (auto_first ? 0 : (page_number % 2)) > 2)
    return Break_node ();

  /* if PAGE-NUMBER is odd, we are starting on a right hand page. That is, we
     have the options
     PAGE-NUMBER odd:
       - even number of pages + a blank page
       - odd number of pages
     PAGE-NUMBER even:
       - odd number of pages + a blank page
       - even number of pages

     No matter which case PAGE-NUMBER falls into, we take the second choice if
     min_p_count has that evenness. (For example, if PAGE-NUMBER is even and
     min_p_count is even, we don't even consider the blank page option). */

  Page_spacing_result result;
  if (start == 0 && auto_first)
    {
      if (min_p_count % 2)
        result = space_systems_on_n_or_one_more_pages (
          configuration, min_p_count, page_number, 0);
      else
        result
          = space_systems_on_n_pages (configuration, min_p_count, page_number);
    }
  else if (((page_number % 2) == 0) == ((min_p_count % 2) == 0))
    result = space_systems_on_n_pages (configuration, min_p_count, page_number);
  else
    result = space_systems_on_n_or_one_more_pages (
      configuration, min_p_count, page_number, blank_page_penalty ());

  Break_node ret;
  ret.prev_ = start - 1;
  ret.break_pos_ = end;
  ret.page_count_ = result.force_.size ();
  ret.first_page_number_ = page_number;
  if (auto_first && (start == 0) && ((ret.page_count_ % 2) == 0))
    ret.first_page_number_ += 1;

  ret.div_ = current_configuration (configuration);
  ret.system_count_ = result.systems_per_page_;

  ret.too_many_lines_ = all_lines_stretched (configuration);
  ret.demerits_ = result.demerits_;
  if (start > 0)
    ret.demerits_ += state_[start - 1].demerits_;

  return ret;
}

/* The number of pages taken up by a Break_node, including
   the blank page if there is one */
vsize
Page_turn_page_breaking::total_page_count (Break_node const &b)
{
  vsize end = b.first_page_number_ + b.page_count_;
  return end - 1 + (end % 2) - b.first_page_number_;
}

extern bool debug_page_breaking_scoring;

void
Page_turn_page_breaking::calc_subproblem (vsize ending_breakpoint)
{
  vsize end = ending_breakpoint + 1;
  Break_node best;
  Break_node cur;
  Break_node this_start_best;
  vsize prev_best_system_count = 0;

  for (vsize start = end; start--;)
    {
      if (start < end - 1
          && scm_is_eq (breakpoint_property (start + 1, "page-turn-permission"),
                        ly_symbol2scm ("force")))
        break;

      if (start > 0 && best.demerits_ < state_[start - 1].demerits_)
        continue;

      int p_num
        = from_scm (book_->paper ()->c_variable ("first-page-number"), 1);
      if (start > 0)
        {
          /* except possibly for the first page, enforce the fact that first_page_number_
             should always be even (left hand page).
             TODO: are there different conventions in right-to-left languages?
          */
          p_num = state_[start - 1].first_page_number_;
          p_num += static_cast<int> (state_[start - 1].page_count_);
          p_num += p_num % 2;
        }

      Line_division min_division;
      Line_division max_division;

      vsize min_sys_count = min_system_count (start, end);
      vsize max_sys_count = max_system_count (start, end);
      this_start_best.demerits_ = infinity_f;

      bool ok_page = true;

      if (debug_page_breaking_scoring)
        {
          message (_f ("page-turn-page-breaking: breaking from %zu to %zu",
                       start, end));
        }

      /* heuristic: we've just added a breakpoint, we'll need at least as
         many systems as before */
      min_sys_count = std::max (min_sys_count, prev_best_system_count);
      for (vsize sys_count = min_sys_count;
           sys_count <= max_sys_count && ok_page; sys_count++)
        {
          set_current_breakpoints (start, end, sys_count, min_division,
                                   max_division);
          bool found = false;

          for (vsize i = 0; i < current_configuration_count (); i++)
            {
              cur = put_systems_on_pages (start, end, i, p_num);

              if (std::isinf (cur.demerits_)
                  || (cur.page_count_ + (p_num % 2) > 2
                      && (!std::isinf (this_start_best.demerits_))
                      && total_page_count (cur)
                           > total_page_count (this_start_best)))
                {
                  ok_page = false;
                  break;
                }

              if (cur.demerits_ < this_start_best.demerits_)
                {
                  if (debug_page_breaking_scoring)
                    print_break_node (cur);

                  found = true;
                  this_start_best = cur;
                  prev_best_system_count = sys_count;

                  /* heuristic: if we increase the number of systems, we can bound the
                     division from below by our current best division */
                  min_division = current_configuration (i);
                }
            }
          if (!found && this_start_best.too_many_lines_)
            break;
        }

      if (std::isinf (this_start_best.demerits_))
        {
          assert (!std::isinf (best.demerits_) && start < end - 1);
          break;
        }

      if (start == 0 && end == 1 && this_start_best.first_page_number_ == 1
          && this_start_best.page_count_ > 1)
        warning (_ ("cannot fit the first page turn onto a single page."
                    "  Consider setting first-page-number to an even number."));

      if (this_start_best.demerits_ < best.demerits_)
        best = this_start_best;
    }
  state_.push_back (best);
}

SCM
Page_turn_page_breaking::solve ()
{
  state_.clear ();
  message (_f ("Calculating page and line breaks (%zu possible page breaks)...",
               last_break_position ()));
  for (vsize i = 0; i < last_break_position (); i++)
    {
      calc_subproblem (i);
      progress_indication (string ("[") + std::to_string (i + 1) + "]");
    }

  vector<Break_node> breaking;
  int i = static_cast<int> (state_.size ()) - 1;
  while (i >= 0)
    {
      breaking.push_back (state_[i]);
      i = static_cast<int> (state_[i].prev_);
    }
  std::reverse (breaking.begin (), breaking.end ());

  message (_ ("Drawing systems..."));
  SCM systems = make_lines (&breaking);
  return make_pages (breaking, systems);
}

/* do the line breaking in all the scores and return a big list of systems */
SCM
Page_turn_page_breaking::make_lines (vector<Break_node> *psoln)
{
  vector<Break_node> &soln = *psoln;
  for (vsize n = 0; n < soln.size (); n++)
    {
      vsize start = n > 0 ? soln[n - 1].break_pos_ : 0;
      vsize end = soln[n].break_pos_;

      break_into_pieces (start, end, soln[n].div_);
    }

  return systems ();
}

SCM
Page_turn_page_breaking::make_pages (vector<Break_node> const &soln,
                                     SCM systems)
{
  if (scm_is_null (systems))
    return SCM_EOL;

  vector<vsize> lines_per_page;
  for (vsize i = 0; i < soln.size (); i++)
    {
      for (vsize j = 0; j < soln[i].page_count_; j++)
        lines_per_page.push_back (soln[i].system_count_[j]);

      if (i + 1 < soln.size ()
          && (soln[i].first_page_number_ + soln[i].page_count_) % 2)
        /* add a blank page */
        lines_per_page.push_back (0);
    }

  /* this should only actually modify first-page-number if
     auto-first-page-number was true. */
  book_->paper ()->set_variable (ly_symbol2scm ("first-page-number"),
                                 to_scm (soln[0].first_page_number_));
  return Page_breaking::make_pages (lines_per_page, systems);
}

void
Page_turn_page_breaking::print_break_node (Break_node const &node)
{
  vsize system_count = 0;
  for (vsize i = 0; i < node.system_count_.size (); i++)
    system_count += node.system_count_[i];

  message (_f ("break starting at page %d", node.first_page_number_));
  message (_f ("\tdemerits: %f", node.demerits_));
  message (_f ("\tsystem count: %zu", system_count));
  message (_f ("\tpage count: %zu", node.page_count_));
  message (_f ("\tprevious break: %zu", node.prev_));
}
