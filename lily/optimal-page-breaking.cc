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
is_break (Grob *g)
{
  return g->get_property ("page-break-permission") == ly_symbol2scm ("force");
}

Optimal_page_breaking::Optimal_page_breaking (Paper_book *pb)
  : Page_breaking (pb, is_break)
{
}

Optimal_page_breaking::~Optimal_page_breaking ()
{
}

// Solves the subproblem betwen the (END-1)th \pageBreak and the
// ENDth \pageBreak.
// Returns a vector of systems per page for the pages within this chunk.
vector<vsize>
Optimal_page_breaking::solve_chunk (vsize end)
{
  vsize max_sys_count = max_system_count (end-1, end);
  vsize first_page_num = robust_scm2int (book_->paper_->c_variable ("first-page-number"), 1);
  SCM forced_page_count = book_->paper_->c_variable ("page-count");

  set_to_ideal_line_configuration (end-1, end);

  Page_spacing_result best;
  vsize page_count = robust_scm2int (forced_page_count, 1);
  Line_division ideal_line_division = current_configuration (0);
  Line_division best_division = ideal_line_division;
  vsize min_sys_count = 1;
  vsize ideal_sys_count = system_count ();
  
  if (!scm_is_integer (forced_page_count))
    {
      if (systems_per_page () > 0)
	best = space_systems_with_fixed_number_per_page (0, first_page_num);
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
      best = space_systems_on_n_pages (0, page_count, first_page_num);
      min_sys_count = page_count;
    }

  if (page_count == 1 || scm_is_integer (forced_page_count))
    progress_indication (_f ("[%d: %d pages]", (int) end, (int) page_count));
  else
    progress_indication (_f ("[%d: %d or %d pages]", (int) end, (int) page_count-1, (int)page_count));

  /* try a smaller number of systems than the ideal number for line breaking */
  Line_division bound = ideal_line_division;
  for (vsize sys_count = ideal_sys_count; --sys_count >= min_sys_count;)
    {
      Page_spacing_result best_for_this_sys_count;
      set_current_breakpoints (end-1, end, sys_count, Line_division (), bound);

      for (vsize i = 0; i < current_configuration_count (); i++)
	{
	  vsize min_p_count = min_page_count (i, first_page_num);
	  Page_spacing_result cur;

	  if (min_p_count == page_count || scm_is_integer (forced_page_count))
	    cur = space_systems_on_n_pages (i, page_count, first_page_num);
	  else
	    cur = space_systems_on_n_or_one_more_pages (i, page_count-1, first_page_num);

	  if (cur.demerits_ < best_for_this_sys_count.demerits_)
	    {
	      best_for_this_sys_count = cur;
	      bound = current_configuration (i);
	    }
	}

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
     is more or less C&P, but the loop bounds make it difficult to try something
     like do {...} while (flip(&d) != UP). */
  bound = ideal_line_division;
  for (vsize sys_count = ideal_sys_count+1; sys_count <= max_sys_count; sys_count++)
    {
      Real best_demerits_for_this_sys_count = infinity_f;
      set_current_breakpoints (end-1, end, sys_count, bound);

      for (vsize i = 0; i < current_configuration_count (); i++)
	{
	  vsize min_p_count = min_page_count (i, first_page_num);
	  Page_spacing_result cur;

	  if (min_p_count > page_count)
	    continue;
	  else
	    cur = space_systems_on_n_pages (i, page_count, first_page_num);

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
      if (best_demerits_for_this_sys_count >= BAD_SPACING_PENALTY
	&& !(best.system_count_status_ & SYSTEM_COUNT_TOO_FEW))
	break;
    }
  break_into_pieces (end-1, end, best_division);

  return best.systems_per_page_;
}

SCM
Optimal_page_breaking::solve ()
{
  vector<vsize> systems_per_page;

  message (_f ("Solving %d page-breaking chunks...", last_break_position ()));
  for (vsize end = 1; end <= last_break_position (); ++end)
    {
      vector<vsize> chunk_systems = solve_chunk (end);
      systems_per_page.insert (systems_per_page.end (), chunk_systems.begin (), chunk_systems.end ());
    }

  message (_ ("Drawing systems..."));
  SCM lines = systems ();
  return make_pages (systems_per_page, lines);
}

