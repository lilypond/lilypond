/*
  optimal-page-breaking.cc -- implement a page-breaker that
  will break pages in such a way that both horizontal and
  vertical spacing will be acceptable

  source file of the GNU LilyPond music typesetter

  (c) 2006--2009 Joe Neeman <joeneeman@gmail.com>
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
is_break (Grob *)
{
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
  vsize end = last_break_position ();
  vsize max_sys_count = max_system_count (0, end);
  vsize first_page_num = robust_scm2int (book_->paper_->c_variable ("first-page-number"), 1);
  SCM forced_page_count = book_->paper_->c_variable ("page-count");
  int systems_per_page = robust_scm2int (book_->paper_->c_variable ("systems-per-page"), 0);

  set_to_ideal_line_configuration (0, end);

  Page_spacing_result best;
  vsize page_count = robust_scm2int (forced_page_count, 1);
  Line_division ideal_line_division = current_configuration (0);
  Line_division best_division = ideal_line_division;
  vsize min_sys_count = 1;
  vsize ideal_sys_count = system_count ();
  
  if (!scm_is_integer (forced_page_count))
    {
      /* find out the ideal number of pages */
      message (_ ("Finding the ideal number of pages..."));
  
      if (systems_per_page > 0)
	best = space_systems_with_fixed_number_per_page (0, first_page_num, systems_per_page);
      else
	best = space_systems_on_best_pages (0, first_page_num);

      page_count = best.systems_per_page_.size ();
      ideal_sys_count = best.system_count ();
      min_sys_count = ideal_sys_count - best.systems_per_page_.back ();
  
      if (page_count > 1 && best.systems_per_page_[page_count - 2] > 1)
	min_sys_count -= best.systems_per_page_[page_count - 2];

      min_sys_count = max (min_sys_count, (vsize)1);
    }
  else
    {
      /* TODO: the following line will spit out programming errors if the
	 ideal line spacing doesn't fit on PAGE_COUNT pages */
      /* TODO: the interaction between systems_per_page and page_count needs to
	 be considered. */
      best = space_systems_on_n_pages (0, page_count, first_page_num, systems_per_page);
      min_sys_count = page_count;
    }

  if (page_count == 1)
    message (_ ("Fitting music on 1 page..."));
  else if (scm_is_integer (forced_page_count))
    message (_f ("Fitting music on %d pages...", (int)page_count));
  else
    message (_f ("Fitting music on %d or %d pages...", (int)page_count-1, (int)page_count));

  /* try a smaller number of systems than the ideal number for line breaking */
  Line_division bound = ideal_line_division;
  for (vsize sys_count = ideal_sys_count; --sys_count >= min_sys_count;)
    {
      Page_spacing_result best_for_this_sys_count;
      set_current_breakpoints (0, end, sys_count, Line_division (), bound);

      for (vsize i = 0; i < current_configuration_count (); i++)
	{
	  vsize min_p_count = min_page_count (i, first_page_num);
	  Page_spacing_result cur;

	  if (min_p_count == page_count || scm_is_integer (forced_page_count))
	    cur = space_systems_on_n_pages (i, page_count, first_page_num, systems_per_page);
	  else
	    cur = space_systems_on_n_or_one_more_pages (i, page_count-1, first_page_num, systems_per_page);

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

      /* if the pages are stretched on average, stop trying to reduce sys_count */
      if (best_for_this_sys_count.page_count () < page_count
	  && best_for_this_sys_count.average_force () > 0)
	break;
	

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
	  Page_spacing_result cur;

	  if (min_p_count > page_count)
	    continue;
	  else
	    cur = space_systems_on_n_pages (i, page_count, first_page_num, systems_per_page);

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

  message (_ ("Drawing systems..."));
  break_into_pieces (0, end, best_division);
  SCM lines = systems ();
  return make_pages (best.systems_per_page_, lines);
}

