/*
  optimal-page-breaking.hh -- implement a page-breaker that
  will break pages in such a way that both horizontal and
  vertical spacing will be acceptable

  source file of the GNU LilyPond music typesetter

  (c) 2006 Joe Neeman <joeneeman@gmail.com>
*/

#include "optimal-page-breaking.hh"
#include "output-def.hh"
#include "page-spacing.hh"
#include "paper-book.hh"
#include "paper-score.hh"
#include "prob.hh"
#include "system.hh"

Optimal_page_breaking::Optimal_page_breaking (Paper_book *pb)
  : Page_breaking (pb, false)
{
}

Optimal_page_breaking::~Optimal_page_breaking ()
{
}

Spacing_result
Optimal_page_breaking::try_page_spacing (vector<vsize> line_count)
{
  vector<Line_details> lines = get_line_details (0, breaks_.size () - 1, line_count);
  Real page_h = page_height (1, false); // FIXME
  SCM force_sym = ly_symbol2scm ("blank-last-page-force");
  Real blank_force = robust_scm2double (book_->paper_->lookup_variable (force_sym), 0);
  Spacing_result ret = space_systems_on_best_pages (lines, page_h, blank_force);

  bool ragged_all = to_boolean (book_->paper_->c_variable ("ragged-bottom"));
  bool ragged_last = to_boolean (book_->paper_->c_variable ("ragged-last-bottom"));

  /* add in the line penalties */
  Real line_force = 0;
  Real line_penalty = 0;
  Real page_weighting = robust_scm2double (book_->paper_->c_variable ("page-spacing-weight"), 1);

  for (vsize i = 0; i < lines.size (); i++)
    {
      line_force += fabs (lines[i].force_);
      line_penalty += lines[i].break_penalty_;
    }

  if (ragged_all)
    for (vsize i = 0; i < ret.force_.size () - 1; i++)
      ret.force_[i] = min (0.0, ret.force_[i]);
  if (ragged_all || ragged_last)
    ret.force_.back () = min (0.0, ret.force_.back ());

  ret.demerits_ = ret.force_[0] * ret.force_[0] * page_weighting;
  for (vsize i = 1; i < ret.force_.size (); i++)
    {
      Real uniformity = fabs (ret.force_[i] - ret.force_[i-1]);
      ret.demerits_ += (ret.force_[i] * ret.force_[i]
		       + uniformity * uniformity) * page_weighting;
    }
  ret.demerits_ += line_force + line_penalty;
  return ret;
}

/* The algorithm is as follows:
   1) break everything into its preferred number of lines
   2) decrease the number of lines until we've decreased
      the number of pages
   3) increase the number of lines until we've increased
      the number of pages
   Take the best score we've found
*/
SCM
Optimal_page_breaking::solve ()
{
  vector<vsize> ideal_line_count;
  vector<vsize> max_line_count;
  vector<vsize> min_line_count;
  vector<vsize> last_best_line_count;
  vector<vsize> best_line_count;
  vsize last_line_total = 0;

  calc_system_count_bounds (0, breaks_.size () - 1, &min_line_count, &max_line_count);
  ideal_line_count.resize (all_.size (), 1);
  for (vsize i = 0; i < all_.size (); i++)
    {
      if (all_[i].pscore_)
	ideal_line_count[i] = line_breaking_[i].get_best_solution (0, VPOS).size ();
      last_line_total += ideal_line_count[i];
    }

  Spacing_result best_result = try_page_spacing (ideal_line_count);
  vsize original_page_count = best_result.systems_per_page_.size ();
  best_line_count = ideal_line_count;
  last_best_line_count = ideal_line_count;

  Direction d = original_page_count > 1 ? DOWN : UP;
  vector<vector<vsize> > div;
  Spacing_result this_best_result;
  do {
    do {
      vector<vsize> blank;

      vector<vsize> this_best_line_count;
      this_best_result.demerits_ = infinity_f;

      last_line_total += d;
      div.clear ();
      divide_systems (last_line_total,
		      d == DOWN ? min_line_count       : last_best_line_count,
		      d == DOWN ? last_best_line_count : max_line_count,
		      &div, &blank);

      for (vsize i = 0; i < div.size (); i++)
	{
	  Spacing_result cur = try_page_spacing (div[i]);
	  if (cur.demerits_ < this_best_result.demerits_)
	    {
	      this_best_result = cur;
	      this_best_line_count = div[i];
	    }
	}
      last_best_line_count = this_best_line_count;

      if (this_best_result.demerits_ < best_result.demerits_)
	{
	  best_line_count = this_best_line_count;
	  best_result = this_best_result;
	}
    } while (div.size () && this_best_result.systems_per_page_.size () == original_page_count);

    /* we're finished decreasing system count, let's try raising it */
    last_best_line_count = ideal_line_count;
    last_line_total = 0;
    for (vsize i = 0; i < ideal_line_count.size (); i++)
      last_line_total += ideal_line_count[i];

  } while (flip (&d) != DOWN);

  SCM lines = make_lines (best_line_count);
  SCM pages = make_pages (best_result.systems_per_page_, lines);
  return pages;
}

SCM
Optimal_page_breaking::make_lines (vector<vsize> line_count)
{
  assert (line_count.size () == all_.size ());

  SCM ret = SCM_EOL;
  for (vsize i = 0; i < all_.size (); i++)
    {
      if (all_[i].pscore_)
	{
	  vector<Column_x_positions> line_brk = line_breaking_[i].get_solution (0, VPOS, line_count[i]);
	  System *sys = all_[i].pscore_->root_system ();

	  sys->break_into_pieces (line_brk);
	  ret = scm_append (scm_list_2 (ret, scm_vector_to_list (sys->get_paper_systems ())));
	}
      else
	{
	  SCM l = scm_cons (all_[i].prob_->self_scm (), SCM_EOL);
	  ret = scm_append (scm_list_2 (ret, l));
	  all_[i].prob_->unprotect ();
	}
    }
  return ret;
}
