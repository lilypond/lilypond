/*
  page-breaking.cc -- implement a superclass and utility
  functions shared by various page-breaking algorithms

  source file of the GNU LilyPond music typesetter

  (c) 2006--2007 Joe Neeman <joeneeman@gmail.com>
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

/* for Page_breaking, the start index (when we are dealing with the stuff
   between a pair of breakpoints) refers to the break_ index of the end of
   the previous page. So the system index of the start of the current page
   could either be the same as the end of the previous page or one more than
   it. */

/* Turn a break index into the sys index that starts the next page */
vsize
Page_breaking::next_system (Break_position const &break_pos) const
{
  vsize sys = break_pos.sys_;

  if (sys == VPOS) /* beginning of the book */
    return 0;
  if (all_[sys].pscore_ && !break_pos.score_ender_)
    return sys; /* the score overflows the previous page */
  return sys + 1; /* this page starts with a new sys */
}

Page_breaking::Page_breaking (Paper_book *pb, Break_predicate is_break)
{
  book_ = pb;
  create_system_list ();
  find_chunks_and_breaks (is_break);
}

Page_breaking::~Page_breaking ()
{
}

/* translate indices into breaks_ into start-end parameters for the line breaker */
void
Page_breaking::line_breaker_args (vsize sys,
				  Break_position const &start,
				  Break_position const &end,
				  vsize *line_breaker_start,
				  vsize *line_breaker_end)
{
  assert (all_[sys].pscore_);
  assert (next_system (start) <= sys && sys <= end.sys_);

  if (start.sys_ == sys)
    *line_breaker_start = start.score_break_;
  else
    *line_breaker_start = 0;

  if (end.sys_ == sys)
    *line_breaker_end = end.score_break_;
  else
    *line_breaker_end = VPOS;
}

void
Page_breaking::break_into_pieces (vsize start_break, vsize end_break, Line_division const &div)
{
  vector<Break_position> chunks = chunk_list (start_break, end_break);
  bool ignore_div = false;
  if (chunks.size () != div.size () + 1)
    {
      programming_error ("did not find a valid page breaking configuration");
      ignore_div = true;
      assert (0);
    }

  for (vsize i = 0; i + 1 < chunks.size (); i++)
    {
      vsize sys = next_system (chunks[i]);
      if (all_[sys].pscore_)
	{
	  vsize start;
	  vsize end;
	  line_breaker_args (sys, chunks[i], chunks[i+1], &start, &end);

	  vector<Column_x_positions> pos = ignore_div
	    ? line_breaking_[sys].get_best_solution (start, end)
	    : line_breaking_[sys].get_solution (start, end, div[i]);
	  all_[sys].pscore_->root_system ()->break_into_pieces (pos);
	}
    }
}

SCM
Page_breaking::systems ()
{
  SCM ret = SCM_EOL;
  for (vsize sys = 0; sys < all_.size (); sys++)
    {
      if (all_[sys].pscore_)
	{
	  all_[sys].pscore_->root_system ()->do_break_substitution_and_fixup_refpoints ();
	  SCM lines = all_[sys].pscore_->root_system ()->get_broken_system_grobs ();
	  ret = scm_cons (lines, ret);
	}
      else
	{
	  Prob *pb = all_[sys].prob_;
	  ret = scm_cons (scm_list_1 (pb->self_scm ()), ret);
	  pb->unprotect ();
	}
    }
  return scm_append (scm_reverse (ret));
}

vector<Line_details>
Page_breaking::line_details (vsize start_break, vsize end_break, Line_division const &div)
{
  vector<Break_position> chunks = chunk_list (start_break, end_break);
  vector<Line_details> ret;
  assert (chunks.size () == div.size () + 1);

  for (vsize i = 0; i + 1 < chunks.size (); i++)
    {
      vsize sys = next_system (chunks[i]);
      if (all_[sys].pscore_)
	{
	  vsize start;
	  vsize end;
	  line_breaker_args (sys, chunks[i], chunks[i+1], &start, &end);

	  vector<Line_details> details = line_breaking_[sys].line_details (start, end, div[i]);
	  ret.insert (ret.end (), details.begin (), details.end ());
	}
      else
	{
	  assert (div[i] == 1);
	  ret.push_back (Line_details (all_[sys].prob_));
	}
    }
  return ret;
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
Page_breaking::breakpoint_property (vsize breakpoint, char const *str)
{
  Break_position const &pos = breaks_[breakpoint];

  if (pos.sys_ == VPOS)
    return SCM_EOL;
  if (all_[pos.sys_].pscore_)
    return pos.col_->get_property (str);
  return all_[pos.sys_].prob_->get_property (str);
}

SCM
Page_breaking::make_pages (vector<vsize> lines_per_page, SCM systems)
{
  SCM layout_module = scm_c_resolve_module ("scm layout-page-layout");
  SCM page_module = scm_c_resolve_module ("scm page");

  SCM make_page = scm_c_module_lookup (layout_module, "stretch-and-draw-page");
  SCM page_stencil = scm_c_module_lookup (page_module, "page-stencil");
  make_page = scm_variable_ref (make_page);
  page_stencil = scm_variable_ref (page_stencil);

  SCM book = book_->self_scm ();
  bool ragged_all = to_boolean (book_->paper_->c_variable ("ragged-bottom"));
  bool ragged_last = to_boolean (book_->paper_->c_variable ("ragged-last-bottom"));
  int first_page_number = robust_scm2int (book_->paper_->c_variable ("first-page-number"), 1);
  SCM ret = SCM_EOL;

  for (vsize i = 0; i < lines_per_page.size (); i++)
    {
      SCM page_num = scm_from_int (i + first_page_number);
      SCM last = scm_from_bool (i == lines_per_page.size () - 1);
      SCM ragged = scm_from_bool (ragged_all || (to_boolean (last) && ragged_last));
      SCM line_count = scm_from_int (lines_per_page[i]);
      SCM lines = scm_list_head (systems, line_count);
      SCM page = scm_apply_0 (make_page,
			      scm_list_n (book, lines, page_num, ragged, last, SCM_UNDEFINED));

      scm_apply_1 (page_stencil, page, SCM_EOL);
      ret = scm_cons (page, ret);
      systems = scm_list_tail (systems, line_count);
    }
  ret = scm_reverse (ret);
  return ret;
}

/* The page-turn-page-breaker needs to have a line-breaker between any two
   columns with non-NULL page-turn-permission.

   The optimal-breaker needs to have a line-breaker between any two columns
   with page-break-permission = 'force.

   By using a grob predicate, we can accommodate both of these uses.
*/
void
Page_breaking::create_system_list ()
{
  SCM specs = book_->get_system_specs ();
  for (SCM s = specs; scm_is_pair (s); s = scm_cdr (s))
    {
      if (Paper_score *ps = dynamic_cast<Paper_score*> (unsmob_music_output (scm_car (s))))
	{
	  SCM system_count = ps->layout ()->c_variable ("system-count");

	  if (scm_is_number (system_count))
	    s = scm_append (scm_list_3 (scm_list_1 (scm_car (s)),
					scm_vector_to_list (ps->get_paper_systems ()),
					scm_cdr (s)));
	  else
	    all_.push_back (System_spec (ps));
	}
      else
        {
          Prob *pb = unsmob_prob (scm_car (s));
          assert (pb);

          pb->protect ();
          all_.push_back (System_spec (pb));
        }
    }
}

void
Page_breaking::find_chunks_and_breaks (Break_predicate is_break)
{
  SCM force_sym = ly_symbol2scm ("force");

  chunks_.push_back (Break_position ());
  breaks_.push_back (Break_position ());

  for (vsize i = 0; i < all_.size (); i++)
    {
      if (all_[i].pscore_)
	{
	  vector<Grob*> cols = all_[i].pscore_->root_system ()->used_columns ();
	  vector<vsize> line_breaker_columns;
	  line_breaker_columns.push_back (0);

	  for (vsize j = 1; j < cols.size (); j++)
	    {
	      bool last = j == cols.size () - 1;
	      bool break_point = is_break (cols[j]);
	      bool chunk_end = cols[j]->get_property ("page-break-permission") == force_sym;
	      Break_position cur_pos = Break_position (i,
						       line_breaker_columns.size (),
						       cols[j],
						       last);

	      if (break_point || (i == all_.size () - 1 && last))
		breaks_.push_back (cur_pos);
	      if (chunk_end || last)
		chunks_.push_back (cur_pos);

	      if ((break_point || chunk_end) && !last)
		line_breaker_columns.push_back (j);
	    }
	  line_breaking_.push_back (Constrained_breaking (all_[i].pscore_, line_breaker_columns));
	}
      else
	{
	  /* TODO: we want some way of applying Break_p to a prob? */
	  if (i == all_.size () - 1)
	    breaks_.push_back (Break_position (i));

	  chunks_.push_back (Break_position (i));
	  line_breaking_.push_back (Constrained_breaking (NULL));
	}
    }
}

vector<Break_position>
Page_breaking::chunk_list (vsize start_index, vsize end_index)
{
  Break_position start = breaks_[start_index];
  Break_position end = breaks_[end_index];

  vsize i;
  for (i = 0; i < chunks_.size () && chunks_[i] <= start; i++)
    ;

  vector<Break_position> ret;
  ret.push_back (start);
  for (; i < chunks_.size () && chunks_[i] < end; i++)
    ret.push_back (chunks_[i]);
  ret.push_back (end);
  return ret;
}

vsize
Page_breaking::min_system_count (vsize start, vsize end)
{
  vector<Break_position> chunks = chunk_list (start, end);
  Line_division div = system_count_bounds (chunks, true);
  vsize ret = 0;

  for (vsize i = 0; i < div.size (); i++)
    ret += div[i];
  return ret;
}

vsize
Page_breaking::max_system_count (vsize start, vsize end)
{
  vector<Break_position> chunks = chunk_list (start, end);
  Line_division div = system_count_bounds (chunks, false);
  vsize ret = 0;

  for (vsize i = 0; i < div.size (); i++)
    ret += div[i];
  return ret;
}

Page_breaking::Line_division
Page_breaking::system_count_bounds (vector<Break_position> const &chunks, bool min)
{
  assert (chunks.size () >= 2);

  Line_division ret;
  ret.resize (chunks.size () - 1, 1);

  for (vsize i = 0; i + 1 < chunks.size (); i++)
    {
      vsize sys = next_system (chunks[i]);
      if (all_[sys].pscore_)
	{
	  vsize start;
	  vsize end;
	  line_breaker_args (sys, chunks[i], chunks[i+1], &start, &end);
	  ret[i] = min
	    ? line_breaking_[sys].min_system_count (start, end)
	    : line_breaking_[sys].max_system_count (start, end);
	}
    }

  return ret;
}

vector<Page_breaking::Line_division>
Page_breaking::line_divisions (vsize start,
			       vsize end,
			       vsize system_count,
			       Line_division lower_bound,
			       Line_division upper_bound)
{
  vector<Break_position> chunks = chunk_list (start, end);

  if (!lower_bound.size ())
    lower_bound = system_count_bounds (chunks, true);
  if (!upper_bound.size ())
    upper_bound = system_count_bounds (chunks, false);

  assert (lower_bound.size () == chunks.size () - 1);
  assert (upper_bound.size () == chunks.size () - 1);

  vector<Line_division> ret;
  Line_division work_in_progress;

  line_divisions_rec (system_count,
		      lower_bound,
		      upper_bound,
		      &ret,
		      &work_in_progress);
  return ret;
}

void
Page_breaking::line_divisions_rec (vsize system_count,
				   Line_division const &min_sys,
				   Line_division const &max_sys,
				   vector<Line_division > *result,
				   Line_division *cur_division)
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
        line_divisions_rec (system_count - i, min_sys, max_sys, result, cur_division);
      cur_division->pop_back ();
    }
}
