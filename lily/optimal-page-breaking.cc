/*
  optimal-page-breaking.cc -- implement a page-breaker that
  will break pages in such a way that both horizontal and
  vertical spacing will be acceptable

  source file of the GNU LilyPond music typesetter

  (c) 2006--2007 Joe Neeman <joeneeman@gmail.com>
*/

#include "international.hh"
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
  vsize min_sys_count = 0;
  vsize ideal_sys_count = 0;
  vsize max_sys_count = max_system_count (0, end);
  vsize page_count = 0;
  Line_division ideal_line_division;
  Line_division best_division;
  Line_division bound;
  vsize first_page_num = robust_scm2int (book_->paper_->c_variable ("first-page-number"), 1);

  /* find out the ideal number of pages */
  message (_ ("Finding the ideal number of pages..."));
  set_to_ideal_line_configuration (0, end);
  ideal_line_division = current_configuration (0);

  Spacing_result best = space_systems_on_best_pages (0, first_page_num);
  page_count = best.systems_per_page_.size ();
  best_division = ideal_line_division;

  for (vsize i = 0; i < page_count; i++)
    ideal_sys_count += best.systems_per_page_[i];

  min_sys_count = ideal_sys_count - best.systems_per_page_.back ();
  if (page_count > 1)
    min_sys_count -= best.systems_per_page_[page_count - 2];

  message (_f ("Fitting music on %d (or one fewer) pages...", (int)page_count));
  /* try a smaller number of systems than the ideal number for line breaking */
  bound = ideal_line_division;
  for (vsize sys_count = ideal_sys_count; --sys_count >= min_sys_count;)
    {
      Spacing_result best_for_this_sys_count;
      set_current_breakpoints (0, end, sys_count, Line_division (), bound);

      for (vsize i = 0; i < current_configuration_count (); i++)
	{
	  vsize min_p_count = min_page_count (i, first_page_num);
	  Spacing_result cur;

	  if (min_p_count > page_count)
	    continue;
	  else if (min_p_count == page_count)
	    cur = space_systems_on_n_pages (i, page_count, first_page_num);
	  else
	    cur = space_systems_on_n_or_one_more_pages (i, page_count-1, first_page_num);

	  if (cur.demerits_ < best_for_this_sys_count.demerits_ || isinf (best_for_this_sys_count.demerits_))
	    {
	      best_for_this_sys_count = cur;
	      bound = current_configuration (i);
	    }
	}

      if (best_for_this_sys_count.demerits_ < best.demerits_ || isinf (best.demerits_))
	{
	  best = best_for_this_sys_count;
	  best_division = bound;
	}

      if (best_for_this_sys_count.systems_per_page_.size () < page_count)
	{
	  /* if the pages are stretched on average, stop trying to reduce sys_count */
	  Real avg_f = 0;
	  for (vsize i = 0; i < best_for_this_sys_count.systems_per_page_.size (); i++)
	    avg_f += best_for_this_sys_count.systems_per_page_[i];
	  if (avg_f > 0)
	    break;
	}

      if (isinf (best_for_this_sys_count.demerits_))
	break;
    }

  /* try a larger number of systems than the ideal line breaking number. This
     is more or less C&P, but the loop bounds make it difficult to try something
     like do {...} while (flip(&d) != UP). */
  bound = ideal_line_division;
  for (vsize sys_count = ideal_sys_count+1; sys_count <= max_sys_count; sys_count++)
    {
      Real best_demerits_for_this_sys_count = infinity_f;
      set_current_breakpoints (0, end, sys_count, bound);

      for (vsize i = 0; i < current_configuration_count (); i++)
	{
	  vsize min_p_count = min_page_count (i, first_page_num);
	  Spacing_result cur;

	  if (min_p_count > page_count)
	    continue;
	  else
	    cur = space_systems_on_n_pages (i, page_count, first_page_num);

	  if (cur.demerits_ < best.demerits_ || isinf (best.demerits_))
	    {
	      best = cur;
	      best_division = current_configuration (i);
	    }

	  if (cur.demerits_ < best_demerits_for_this_sys_count || isinf (best_demerits_for_this_sys_count))
	    {
	      best_demerits_for_this_sys_count = cur.demerits_;
	      bound = current_configuration (i);
	    }
	}
      if (isinf (best_demerits_for_this_sys_count))
	break;
    }

  message ("Drawing systems...");
  break_into_pieces (0, end, best_division);
  SCM lines = systems ();
  return make_pages (best.systems_per_page_, lines);
}

