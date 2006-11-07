/*
  optimal-page-breaking.cc -- implement a page-breaker that
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

Spacing_result
Optimal_page_breaking::try_page_spacing (Line_division const &line_count)
{
  vector<Line_details> lines = line_details (0, breaks_.size () - 1, line_count);
  Real page_h = page_height (1, false); // FIXME
  SCM force_sym = ly_symbol2scm ("blank-last-page-force");
  Real blank_force = robust_scm2double (book_->paper_->lookup_variable (force_sym), 0);
  bool ragged_all = to_boolean (book_->paper_->c_variable ("ragged-bottom"));
  bool ragged_last = to_boolean (book_->paper_->c_variable ("ragged-last-bottom"));
  Spacing_result ret = space_systems_on_best_pages (lines,
						    page_h,
						    blank_force,
						    ragged_all,
						    ragged_last);

  /* add in the line penalties */
  Real line_force = 0;
  Real line_penalty = 0;
  Real page_weighting = robust_scm2double (book_->paper_->c_variable ("page-spacing-weight"), 10);

  for (vsize i = 0; i < lines.size (); i++)
    {
      line_force += lines[i].force_ * lines[i].force_;
      line_penalty += lines[i].break_penalty_;
    }

  ret.demerits_ = ret.force_[0] * ret.force_[0] * page_weighting;
  for (vsize i = 1; i < ret.force_.size (); i++)
    ret.demerits_ += ret.force_[i] * ret.force_[i] * page_weighting;

  /* for a while we tried averaging page and line forces instead of summing
     them, but it caused the following problem. If there is a single page
     with a very bad page force (for example because of a forced page break),
     the page breaker will put in a _lot_ of pages so that the bad force
     becomes averaged out over many pages. */
  ret.demerits_ += line_force + line_penalty;
  return ret;
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

  for (vsize sys_count = min_sys_count;
       cur_page_count <= max_page_count && sys_count <= max_sys_count;
       sys_count++)
    {
      Real this_best_demerits = infinity_f;
      vector<Line_division> div = line_divisions (0, end, sys_count, lower_bound);
      for (vsize d = 0; d < div.size (); d++)
	{
	  Spacing_result cur = try_page_spacing (div[d]);
	  cur_page_count = cur.systems_per_page_.size ();
	  if (cur.demerits_ < best.demerits_ || isinf (best.demerits_))
	    {
	      best = cur;
	      best_division = div[d];
	    }

	  if (cur.demerits_ < this_best_demerits || isinf (best.demerits_))
	    {
	      this_best_demerits = cur.demerits_;
	      lower_bound = div[d];
	    }

	  vector<Line_details> det = line_details (0, end, div[d]);
	  bool all_lines_stretched = true;
	  for (vsize i = 0; i < det.size (); i++)
	    if (det[i].force_ < 0)
	      all_lines_stretched = false;

	  if (all_lines_stretched)
	    max_page_count = min (max_page_count, cur_page_count + 1);
	}
    }

  break_into_pieces (0, end, best_division);
  SCM lines = systems ();
  return make_pages (best.systems_per_page_, lines);
}

