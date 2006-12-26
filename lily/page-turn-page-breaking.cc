/*
  page-turn-page-breaking.cc -- implement Page_turn_page_breaking

  source file of the GNU LilyPond music typesetter

  (c) 2006 Joe Neeman <joeneeman@gmail.com>
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

static bool
is_break (Grob *g)
{
  return scm_is_symbol (g->get_property ("page-turn-permission"));
}

Page_turn_page_breaking::Page_turn_page_breaking (Paper_book *pb)
  : Page_breaking (pb, is_break)
{
}

Page_turn_page_breaking::~Page_turn_page_breaking ()
{
}

Page_turn_page_breaking::Break_node
Page_turn_page_breaking::put_systems_on_pages (vsize start,
					       vsize end,
					       vector<Line_details> const &lines,
					       Line_division const &div,
					       int page_number)
{
  bool last = end == breaks_.size () - 1;
  bool ragged_all = to_boolean (book_->paper_->c_variable ("ragged-bottom"));
  bool ragged_last = last && to_boolean (book_->paper_->c_variable ("ragged-last-bottom"));
  Real page_h = page_height (1, false); // FIXME
  SCM force_sym = last ? ly_symbol2scm ("blank-last-page-force") : ly_symbol2scm ("blank-page-force");
  Real blank_force = robust_scm2double (book_->paper_->lookup_variable (force_sym), 0);
  Real page_weighting = robust_scm2double (book_->paper_->c_variable ("page-spacing-weight"), 10);
  int min_p_count = min_page_count (lines, page_h, ragged_all, ragged_last);
  bool auto_first = to_boolean (book_->paper_->c_variable ("auto-first-page-number"));

  /* If [START, END] does not contain an intermediate
     breakpoint, we may need to consider solutions that result in a bad turn.
     In this case, we won't abort if the min_page_count is too big */
  if (start < end - 1 && min_p_count > 2)
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

  Spacing_result result;
  if (start == 0 && auto_first)
    {
      if (min_p_count % 2)
	result = space_systems_on_n_or_one_more_pages (lines, min_p_count, page_h, 0, ragged_all, ragged_last);
      else
	result = space_systems_on_n_pages (lines, min_p_count, page_h, ragged_all, ragged_last);
    }
  else if (page_number % 2 == min_p_count % 2)
    result = space_systems_on_n_pages (lines, min_p_count, page_h, ragged_all, ragged_last);
  else
    result = space_systems_on_n_or_one_more_pages (lines, min_p_count, page_h, blank_force, ragged_all, ragged_last);

  Break_node ret;
  ret.prev_ = start - 1;
  ret.break_pos_ = end;
  ret.page_count_ = result.force_.size ();
  ret.first_page_number_ = page_number;
  if (auto_first && start == 0)
    ret.first_page_number_ += 1 - (ret.page_count_ % 2);

  ret.div_ = div;
  ret.system_count_ = result.systems_per_page_;

  ret.too_many_lines_ = true;
  ret.demerits_ = result.penalty_;
  if (start > 0)
    ret.demerits_ += state_[start-1].demerits_;
  for (vsize i = 0; i < lines.size (); i++)
    {
      ret.demerits_ += lines[i].force_ * lines[i].force_;
      ret.demerits_ += lines[i].break_penalty_;
      if (lines[i].force_ < 0)
	ret.too_many_lines_ = false;
    }
  for (vsize i = 0; i < result.force_.size (); i++)
    ret.demerits_ += result.force_[i] * result.force_[i] * page_weighting;
  return ret;
}

/* "final page" meaning the number of the final right-hand page,
   which always has an odd page number */
vsize
Page_turn_page_breaking::final_page_num (Break_node const &b)
{
  vsize end = b.first_page_number_ + b.page_count_;
  return end + 1 - (end % 2);
}

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
      if (start < end-1
	  && breakpoint_property (start+1, "page-turn-permission") == ly_symbol2scm ("force"))
	break;

      if (start > 0 && best.demerits_ < state_[start-1].demerits_)
        continue;

      int p_num = robust_scm2int (book_->paper_->c_variable ("first-page-number"), 1);
      if (start > 0)
        {
	  /* except possibly for the first page, enforce the fact that first_page_number_
	     should always be even (left hand page).
	     TODO: are there different conventions in right-to-left languages?
	  */
	  p_num = state_[start-1].first_page_number_ + state_[start-1].page_count_;
	  p_num += p_num % 2;
        }

      Line_division min_division;
      Line_division max_division;

      vsize min_sys_count = min_system_count (start, end);
      vsize max_sys_count = max_system_count (start, end);
      this_start_best.demerits_ = infinity_f;

      bool ok_page = true;

      /* heuristic: we've just added a breakpoint, we'll need at least as
         many systems as before */
      min_sys_count = max (min_sys_count, prev_best_system_count);
      for (vsize sys_count = min_sys_count; sys_count <= max_sys_count && ok_page; sys_count++)
        {
	  vector<Line_division> div = line_divisions (start, end, sys_count, min_division, max_division);
          bool found = false;

          for (vsize d = 0; d < div.size (); d++)
            {
	      vector<Line_details> line = line_details (start, end, div[d]);

              cur = put_systems_on_pages (start, end, line, div[d], p_num);

              if (isinf (cur.demerits_)
		  || (cur.page_count_ > 2
		      && (!isinf (this_start_best.demerits_))
		      && final_page_num (cur) > final_page_num (this_start_best)))
                {
                  ok_page = false;
                  break;
                }

              if (cur.demerits_ < this_start_best.demerits_)
                {
                  found = true;
                  this_start_best = cur;
                  prev_best_system_count = sys_count;

		  /* heuristic: if we increase the number of systems, we can bound the
		     division from below by our current best division */
		  min_division = div[d];
                }
            }
          if (!found && this_start_best.too_many_lines_)
            break;
        }
      if (isinf (this_start_best.demerits_))
        {
          assert (!isinf (best.demerits_) && start < end - 1);
          break;
        }

      if (start == 0 && end == 1
	  && this_start_best.first_page_number_ == 1
	  && this_start_best.page_count_ > 1)
	warning (_ ("cannot fit the first page turn onto a single page.  "
		    "Consider setting first-page-number to an even number."));

      if (this_start_best.demerits_ < best.demerits_)
	best = this_start_best;
    }
  state_.push_back (best);
}

SCM
Page_turn_page_breaking::solve ()
{
  state_.clear ();
  message (_f ("Calculating page and line breaks (%d possible page breaks)...",
               (int)breaks_.size () - 1) + " ");
  for (vsize i = 0; i < breaks_.size () - 1; i++)
    {
      calc_subproblem (i);
      progress_indication (string ("[") + to_string (i + 1) + "]");
    }
  progress_indication ("\n");

  vector<Break_node> breaking;
  int i = state_.size () - 1;
  while (i >= 0)
    {
      breaking.push_back (state_[i]);
      i = state_[i].prev_;
    }
  reverse (breaking);

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
      vsize start = n > 0 ? soln[n-1].break_pos_ : 0;
      vsize end = soln[n].break_pos_;

      break_into_pieces (start, end, soln[n].div_);
    }

  return systems ();
}

SCM
Page_turn_page_breaking::make_pages (vector<Break_node> const &soln, SCM systems)
{
  vector<vsize> lines_per_page;
  for (vsize i = 0; i < soln.size (); i++)
    {
      for (vsize j = 0; j < soln[i].page_count_; j++)
	lines_per_page.push_back (soln[i].system_count_[j]);

      if (i < soln.size () - 1 && (soln[i].first_page_number_ + soln[i].page_count_) % 2)
	/* add a blank page */
	lines_per_page.push_back (0);
    }

  /* this should only actually modify first-page-number if
     auto-first-page-number was true. */
  book_->paper_->set_variable (ly_symbol2scm ("first-page-number"),
			       scm_from_int (soln[0].first_page_number_));
  return Page_breaking::make_pages (lines_per_page, systems);
}
