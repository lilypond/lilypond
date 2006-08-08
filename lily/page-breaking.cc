/*
  page-breaking.cc -- implement a superclass and utility
  functions for use by various page-breaking algorithms

  source file of the GNU LilyPond music typesetter

  (c) 2006 Joe Neeman <joeneeman@gmail.com>
*/

#include "page-breaking.hh"

#include "international.hh"
#include "item.hh"
#include "output-def.hh"
#include "page-spacing.hh"
#include "paper-book.hh"
#include "paper-score.hh"
#include "paper-system.hh"
#include "system.hh"
#include "warn.hh"

System_spec::System_spec (Paper_score *ps, int break_count)
{
  pscore_ = ps;
  prob_ = 0;
  score_break_count_ = break_count;
}

System_spec::System_spec (Prob *pb)
{
  pscore_ = 0;
  prob_ = pb;
  score_break_count_ = 0;
}

System_spec::System_spec ()
{
  pscore_ = 0;
  prob_ = 0;
  score_break_count_ = 0;
}

/* for Page_breaking, the start index (when we are dealing with the stuff
   between a pair of breakpoints) refers to the break_ index of the end of
   the previous page. So the system index of the start of the current page
   could either be the same as the end of the previous page or one more than
   it. */

/* Turn a break index into the sys index that starts the next page */
vsize Page_breaking::next_system (vsize start) const
{
  vsize sys = breaks_[start].sys_;

  if (sys == VPOS) /* beginning of the piece */
    return 0;
  if (all_[sys].pscore_ && all_[sys].score_break_count_ > breaks_[start].score_break_)
    return sys; /* the score overflows the previous page */
  return sys + 1; /* this page starts with a new sys */
}

Page_breaking::Page_breaking (Paper_book *pb, bool allow_intra_score_breaks)
{
  book_ = pb;
  create_system_list (allow_intra_score_breaks);
}

Page_breaking::~Page_breaking ()
{
}

/* translate indices into breaks_ into start-end parameters for the line breaker */
void
Page_breaking::line_breaker_args (vsize i, vsize *start, vsize *end)
{
  assert (all_[i].pscore_);
  assert (next_system (*start) <= i && i <= breaks_[*end].sys_);

  vsize start_col = 0;
  vsize end_col = VPOS;

  if (breaks_[*start].sys_ == i)
    start_col = breaks_[*start].score_break_;
  if (breaks_[*end].sys_ == i)
    end_col = breaks_[*end].score_break_;

  assert (end_col && (end_col == VPOS || end_col <= all_[breaks_[*end].sys_].score_break_count_));
  *start = start_col;
  *end = end_col;
}

vector<Column_x_positions>
Page_breaking::get_line_breaks (vsize i, vsize sys_count, vsize start, vsize end)
{
  assert (all_[i].pscore_);
  line_breaker_args (i, &start, &end);
  return line_breaking_[i].get_solution (start, end, sys_count);
}

vector<Line_details>
Page_breaking::get_line_details (vsize start_break, vsize end_break, vector<vsize> const &div)
{
  vector<Line_details> ret;

  for (vsize i = next_system (start_break); i <= breaks_[end_break].sys_; i++)
    {
      if (all_[i].pscore_)
	{
	  vsize div_index = i - next_system (start_break);
	  vsize start = start_break;
	  vsize end = end_break;

	  line_breaker_args (i, &start, &end);
	  vector<Line_details> l = line_breaking_[i].get_details (start, end, div[div_index]);
	  ret.insert (ret.end (), l.begin (), l.end ());
	}
      else
	ret.push_back (Line_details (all_[i].prob_));
    }
  return ret;
}

vsize
Page_breaking::get_min_systems (vsize i, vsize start, vsize end)
{
  line_breaker_args (i, &start, &end);
  return line_breaking_[i].get_min_systems (start, end);
}

vsize
Page_breaking::get_max_systems (vsize i, vsize start, vsize end)
{
  line_breaker_args (i, &start, &end);
  return line_breaking_[i].get_max_systems (start, end);
}

Real
Page_breaking::page_height (int page_num, bool last)
{
  SCM mod = scm_c_resolve_module ("scm page");
  SCM calc_height = scm_c_module_lookup (mod, "calc-printable-height");
  SCM make_page = scm_c_module_lookup (mod, "make-page");

  calc_height = scm_variable_ref (calc_height);
  make_page = scm_variable_ref (make_page);

  SCM page = scm_apply_0 (make_page, scm_list_n (
                  book_->self_scm (),
                  ly_symbol2scm ("page-number"), scm_from_int (page_num),
                  ly_symbol2scm ("is-last"), scm_from_bool (last),
                  SCM_UNDEFINED));
  SCM height = scm_apply_1 (calc_height, page, SCM_EOL);
  return scm_to_double (height) - scm_to_double (book_->paper_->c_variable ("page-top-space"));
}

SCM
Page_breaking::make_pages (vector<vsize> lines_per_page, SCM systems)
{
  SCM module = scm_c_resolve_module ("scm layout-page-layout");
  SCM make_page = scm_c_module_lookup (module, "make-page-from-systems");
  make_page = scm_variable_ref (make_page);
  SCM book = book_->self_scm ();
  bool ragged_all = to_boolean (book_->paper_->c_variable ("ragged-bottom"));
  bool ragged_last = to_boolean (book_->paper_->c_variable ("ragged-last-bottom"));
  SCM ret = SCM_EOL;

  for (vsize i = 0; i < lines_per_page.size (); i++)
    {
      SCM page_num = scm_from_int (i + 1);
      SCM last = scm_from_bool (i == lines_per_page.size () - 1);
      SCM ragged = scm_from_bool (ragged_all || (to_boolean (last) && ragged_last));
      SCM line_count = scm_from_int (lines_per_page[i]);
      SCM lines = scm_list_head (systems, line_count);
      SCM page = scm_apply_0 (make_page,
			      scm_list_n (book, lines, page_num, ragged, last, SCM_UNDEFINED));

      ret = scm_cons (page, ret);
      systems = scm_list_tail (systems, line_count);
    }
  return scm_reverse (ret);
}

/* if allow_intra_score_breaks is false, that doesn't necessarily mean that there will
   be no page turns in the middle of a score, only that we don't give special
   consideration to any internal part of a score.

   Corollary: if allow_intra_score_breaks is false, any \pageTurn or \noPageTurn commands
   in the middle of a score will be ignored.
*/
void
Page_breaking::create_system_list (bool allow_intra_score_breaks)
{
  breaks_.push_back (Break_position ());

  SCM specs = book_->get_system_specs ();
  for (SCM s = specs; s != SCM_EOL; s = scm_cdr (s))
    {
      if (Paper_score *ps = dynamic_cast<Paper_score*> (unsmob_music_output (scm_car (s))))
        {
          /* add a breakpoint at the end of the last score, if necessary */
          if (all_.size () && all_.back ().pscore_)
            breaks_.push_back (Break_position (all_.size () - 1,
                                               all_.back ().score_break_count_));

          vector<vsize> score_brk;
	  if (allow_intra_score_breaks)
	    score_brk = find_page_break_indices (ps);

          all_.push_back (System_spec (ps, score_brk.size () + 1));

          for (vsize i = 0; i < score_brk.size(); i++)
            breaks_.push_back (Break_position (all_.size () - 1, i + 1));

          /* include a line breaker at the start of the score */
          score_brk.insert (score_brk.begin (), 0);
          line_breaking_.push_back (Constrained_breaking (score_brk));
          line_breaking_.back ().set_pscore (ps);
        }
      else
        {
          Prob *pb = unsmob_prob (scm_car (s));
          assert (pb);

          pb->protect ();
          // ignore penalties (from break-before) in favour of using \pageBreak at the
          // end of the score
          if (all_.size () && all_.back ().pscore_)
            breaks_.push_back (Break_position (all_.size () - 1, all_.back ().score_break_count_));
          all_.push_back (System_spec (pb));
          line_breaking_.push_back (Constrained_breaking ());
        }
    }

  /* add the ending breakpoint */
  if (all_.back ().pscore_)
    breaks_.push_back (Break_position (all_.size () - 1, all_.back ().score_break_count_));
  else
    breaks_.push_back (Break_position (all_.size () - 1));
}

vector<vsize>
Page_breaking::find_page_break_indices (Paper_score *pscore)
{
  vector<Grob*> all = pscore->root_system ()->columns ();
  vector<vsize> ret;

  /* we don't include breaks at the beginning and end */
  for (vsize i = 0; i < all.size () - 1; i++)
    if (scm_is_symbol (all[i]->get_property ("page-turn-permission")))
      ret.push_back(i);

  return ret;
}

void
Page_breaking::calc_system_count_bounds (vsize start, vsize end,
                                            vector<vsize> *min,
                                            vector<vsize> *max)
{
  for (vsize i = next_system (start); i <= breaks_[end].sys_; i++)
    {
      if (all_[i].pscore_)
        {
          min->push_back (get_min_systems (i, start, end));
          max->push_back (get_max_systems (i, start, end));
        }
      else
        {
          min->push_back (1);
          max->push_back (1);
        }
    }
}

/* calculate all possible ways of dividing system_count between the System_specs */
void
Page_breaking::divide_systems (vsize system_count,
			       vector<vsize> const &min_sys,
			       vector<vsize> const &max_sys,
			       vector<vector<vsize> > *result,
			       vector<vsize> *cur_division)
{
  vsize my_index = cur_division->size ();
  vsize others_min = 0;
  vsize others_max = 0;

  for (vsize i = my_index + 1; i < min_sys.size (); i++)
    {
      others_min += min_sys[i];
      others_max += max_sys[i];
    }
  others_max = min (others_max, system_count);
  vsize real_min = max (min_sys[my_index], system_count - others_max);
  vsize real_max = min (max_sys[my_index], system_count - others_min);

  if (real_min > real_max || real_min <= 0)
    {
      /* this should never happen within a recursive call. If it happens
	 at all, it means that we were called with an unsolvable problem
	 and we should return an empty result */
      assert (my_index == 0);
      return;
    }

  for (vsize i = real_min; i <= real_max; i++)
    {
      cur_division->push_back (i);
      if (my_index == min_sys.size () - 1)
        result->push_back (*cur_division);
      else
        divide_systems (system_count - i, min_sys, max_sys, result, cur_division);
      cur_division->pop_back ();
    }
}

