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

#include "international.hh"
#include "optimal-page-breaking.hh"
#include "output-def.hh"
#include "page-spacing.hh"
#include "paper-book.hh"
#include "paper-score.hh"
#include "prob.hh"
#include "system.hh"

Optimal_page_breaking::Optimal_page_breaking (Paper_book *pb)
  : Page_breaking (pb, 0, 0)
{
}

Optimal_page_breaking::~Optimal_page_breaking ()
{
}

extern bool debug_page_breaking_scoring;

SCM
Optimal_page_breaking::solve ()
{
  vsize end = last_break_position ();
  vsize max_sys_count = max_system_count (0, end);
  int first_page_num
    = from_scm (book_->paper ()->c_variable ("first-page-number"), 1);

  set_to_ideal_line_configuration (0, end);

  Page_spacing_result best;
  SCM forced_page_count = book_->paper ()->c_variable ("page-count");
  bool page_count_is_forced = scm_is_integer (forced_page_count);
  vsize page_count;
  Line_division ideal_line_division = current_configuration (0);
  Line_division best_division = ideal_line_division;
  vsize min_sys_count = 1;

  // Note that system_count () only counts non-title systems.
  vsize ideal_sys_count = system_count ();

  if (page_count_is_forced)
    {
      if (!from_scm<bool> (scm_positive_p (forced_page_count)))
        {
          warning (_ ("page-count must be positive"));
          page_count_is_forced = false;
        }
      else
        {
          page_count = from_scm<vsize> (forced_page_count);
        }
    }

  if (page_count_is_forced)
    {
      /* page_count has been initialized in the check above. */

      /* If systems-per-page and page-count are both specified, we know exactly
         how many systems should be present. */
      if (systems_per_page () > 0)
        {
          ideal_sys_count = systems_per_page () * page_count;

          if (ideal_sys_count > max_system_count (0, end)
              || ideal_sys_count < min_system_count (0, end))
            {
              warning (_ ("could not satisfy systems-per-page and page-count "
                          "at the same time, ignoring systems-per-page"));
              ideal_sys_count = system_count ();
              min_sys_count = page_count;
            }
          else
            {
              set_current_breakpoints (0, end, ideal_sys_count);
              min_sys_count = max_sys_count = ideal_sys_count;
              ideal_line_division = best_division = current_configuration (0);
            }
        }
      else
        min_sys_count = page_count;

      /* TODO: the following line will spit out programming errors if the
         ideal line spacing doesn't fit on PAGE_COUNT pages */
      best = space_systems_on_n_pages (0, page_count, first_page_num);
    }
  else
    {
      /* find out the ideal number of pages */
      message (_ ("Finding the ideal number of pages..."));

      best = space_systems_on_best_pages (0, first_page_num);

      page_count = best.systems_per_page_.size ();
      if (page_count == 0)
        {
          min_sys_count = 0;
        }
      else
        {
          min_sys_count = ideal_sys_count - best.systems_per_page_.back ();

          if (page_count > 1 && best.systems_per_page_[page_count - 2] > 1)
            min_sys_count -= best.systems_per_page_[page_count - 2];

          if (min_sys_count > ideal_sys_count // subtraction wrapped around
              || min_sys_count <= 0)
            min_sys_count = 1;
        }
    }

  if (page_count == 1)
    message (_ ("Fitting music on 1 page..."));
  else if (scm_is_integer (forced_page_count) || page_count == 0)
    message (_f ("Fitting music on %zu pages...", page_count));
  else
    message (
      _f ("Fitting music on %zu or %zu pages...", page_count - 1, page_count));

  /* try a smaller number of systems than the ideal number for line breaking */
  Line_division bound = ideal_line_division;
  for (vsize sys_count = ideal_sys_count + 1; --sys_count >= min_sys_count;)
    {
      Page_spacing_result best_for_this_sys_count;
      set_current_breakpoints (0, end, sys_count, Line_division (), bound);

      if (debug_page_breaking_scoring)
        message (_f ("trying %zu systems", sys_count));

      for (vsize i = 0; i < current_configuration_count (); i++)
        {
          Page_spacing_result cur;

          if (scm_is_integer (forced_page_count))
            cur = space_systems_on_n_pages (i, page_count, first_page_num);
          else
            cur = space_systems_on_best_pages (i, first_page_num);

          if (cur.demerits_ < best_for_this_sys_count.demerits_)
            {
              best_for_this_sys_count = cur;
              bound = current_configuration (i);
            }
        }

      if (debug_page_breaking_scoring)
        message (_f ("best score for this sys-count: %f",
                     best_for_this_sys_count.demerits_));

      if (best_for_this_sys_count.demerits_ < best.demerits_)
        {
          best = best_for_this_sys_count;
          best_division = bound;
        }

      /* Check to see if we already have too few systems. There are two ways
         we check this: if we are trying one less than the ideal number of pages
         and the pages are stretched on average then we have too
         few systems. If the spacing is worse than BAD_SPACING_PENALTY, then we
         have too few systems. In either case, though, we need to continue reducing
         the number of systems if max-systems-per-page requires it. */
      if (!(best.system_count_status_ & SYSTEM_COUNT_TOO_MANY))
        {
          if (best_for_this_sys_count.page_count () < page_count
              && best_for_this_sys_count.average_force () > 0)
            break;

          if (best_for_this_sys_count.demerits_ >= BAD_SPACING_PENALTY)
            break;
        }
    }

  /* try a larger number of systems than the ideal line breaking number. This
     is more or less C&P. */
  bound = ideal_line_division;
  vsize prev_actual_sys_count = 0;
  for (vsize sys_count = ideal_sys_count + 1; sys_count <= max_sys_count;
       sys_count++)
    {
      Real best_demerits_for_this_sys_count = infinity_f;
      set_current_breakpoints (0, end, sys_count, bound);

      if (debug_page_breaking_scoring)
        message (_f ("trying %zu systems", sys_count));

      for (vsize i = 0; i < current_configuration_count (); i++)
        {
          vsize min_p_count = min_page_count (i, first_page_num);
          Page_spacing_result cur;

          if (min_p_count > page_count)
            continue;
          else if (scm_is_integer (forced_page_count))
            cur = space_systems_on_n_pages (i, page_count, first_page_num);
          else
            cur = space_systems_on_best_pages (i, first_page_num);

          if (cur.demerits_ < best.demerits_)
            {
              best = cur;
              best_division = current_configuration (i);
            }

          if (cur.demerits_ < best_demerits_for_this_sys_count)
            {
              best_demerits_for_this_sys_count = cur.demerits_;
              bound = current_configuration (i);
            }
        }

      if (debug_page_breaking_scoring)
        message (_f ("best score for this sys-count: %f",
                     best_demerits_for_this_sys_count));

      vsize actual_sys_count = 0;
      for (const auto &v : best.systems_per_page_)
        actual_sys_count += v;
      // If results are infinitely bad, break if either:
      // - we don't have too few systems (i.e. we have a previous good result
      //   and there is no point trying more and more systems at this point)
      // - we have too few systems, but we also can't increase the system count
      //   (i.e. increasing sys_count isn't actually resulting in there being
      //   more systems for some reason)
      if (best_demerits_for_this_sys_count >= BAD_SPACING_PENALTY
          && (!(best.system_count_status_ & SYSTEM_COUNT_TOO_FEW)
              || actual_sys_count == prev_actual_sys_count))
        break;
      prev_actual_sys_count = actual_sys_count;
    }

  message (_ ("Drawing systems..."));
  break_into_pieces (0, end, best_division);
  SCM lines = systems ();
  return make_pages (best.systems_per_page_, lines);
}
