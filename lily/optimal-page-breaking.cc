/*
  optimal-page-breaking.cc -- implement a page-breaker that
  will break pages in such a way that both horizontal and
  vertical spacing will be acceptable

  source file of the GNU LilyPond music typesetter

  (c) 2006--2007 Joe Neeman <joeneeman@gmail.com>
*/

#include "optimal-page-breaking.hh"
#include "output-def.hh"
#include "page-spacing.hh"
#include "paper-book.hh"
#include "paper-score.hh"
#include "prob.hh"
#include "system.hh"

static bool
is_break (Grob *g)
{
  (void) g; /* shutup warning */
  return false;
}

Optimal_page_breaking::Optimal_page_breaking (Paper_book *pb)
  : Page_breaking (pb, is_break)
{
}

Optimal_page_breaking::~Optimal_page_breaking ()
{
}

SCM
Optimal_page_breaking::solve ()
{
  vsize end = breaks_.size () - 1;
  vsize min_sys_count = min_system_count (0, end);
  vsize max_sys_count = max_system_count (0, end);
  vsize max_page_count = INT_MAX;
  vsize cur_page_count = 0;
  Spacing_result best;
  Line_division best_division;
  Line_division lower_bound;
  vsize first_page_num = robust_scm2int (book_->paper_->c_variable ("first-page-number"), 1);

  for (vsize sys_count = min_sys_count;
       cur_page_count <= max_page_count && sys_count <= max_sys_count;
       sys_count++)
    {
      Real best_demerits_for_this_sys_count = infinity_f;
      set_current_breakpoints (0, end, sys_count, lower_bound);

      for (vsize i = 0; i < current_configuration_count (); i++)
	{
	  Spacing_result cur = space_systems_on_best_pages (i, first_page_num);
	  cur_page_count = cur.systems_per_page_.size ();
	  if (cur.demerits_ < best.demerits_ || isinf (best.demerits_))
	    {
	      best = cur;
	      best_division = current_configuration (i);
	    }

	  if (cur.demerits_ < best_demerits_for_this_sys_count || isinf (best.demerits_))
	    {
	      best_demerits_for_this_sys_count = cur.demerits_;
	      lower_bound = current_configuration (i);
	    }

	  if (all_lines_stretched (i))
	    max_page_count = min (max_page_count, cur_page_count + 1);
	}
    }

  break_into_pieces (0, end, best_division);
  SCM lines = systems ();
  return make_pages (best.systems_per_page_, lines);
}

