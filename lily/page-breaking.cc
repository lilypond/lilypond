/*
  page-breaking.cc -- implement a superclass and utility
  functions shared by various page-breaking algorithms

  source file of the GNU LilyPond music typesetter

  (c) 2006--2009 Joe Neeman <joeneeman@gmail.com>
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

/* for each forbidden page break, merge the systems around it into one
   system. */
static vector<Line_details>
compress_lines (const vector<Line_details> &orig)
{
  vector<Line_details> ret;

  for (vsize i = 0; i < orig.size (); i++)
    {
      if (ret.size () && !scm_is_symbol (ret.back ().page_permission_))
	{
	  Line_details const &old = ret.back ();
	  Line_details compressed = orig[i];
	  compressed.extent_[DOWN] = old.extent_[DOWN];
	  compressed.extent_[UP] = old.extent_[UP] + orig[i].extent_.length () + old.padding_;
	  compressed.space_ += old.space_;
	  compressed.inverse_hooke_ += old.inverse_hooke_;

	  compressed.compressed_lines_count_ = old.compressed_lines_count_ + 1;
	  compressed.compressed_nontitle_lines_count_ =
	    old.compressed_nontitle_lines_count_ + (compressed.title_ ? 0 : 1);

	  compressed.title_ = compressed.title_ && old.title_;
	  ret.back () = compressed;
	}
      else
	{
	  ret.push_back (orig[i]);
	  ret.back ().force_ = 0;
	}
    }
  return ret;
}

/* translate the number of systems-per-page into something meaningful for
   the uncompressed lines.
*/
static vector<vsize>
uncompress_solution (vector<vsize> const &systems_per_page,
		     vector<Line_details> const &compressed)
{
  vector<vsize> ret;
  vsize start_sys = 0;

  for (vsize i = 0; i < systems_per_page.size (); i++)
    {
      int compressed_count = 0;
      for (vsize j = start_sys; j < start_sys + systems_per_page[i]; j++)
	compressed_count += compressed[j].compressed_lines_count_ - 1;

      ret.push_back (systems_per_page[i] + compressed_count);
      start_sys += systems_per_page[i];
    }
  return ret;
}

/* for Page_breaking, the start index (when we are dealing with the stuff
   between a pair of breakpoints) refers to the break_ index of the end of
   the previous page. So the system index of the start of the current page
   could either be the same as the end of the previous page or one more than
   it. */

/* Turn a break index into the sys index that starts the next page */
vsize
Page_breaking::next_system (Break_position const &break_pos) const
{
  vsize sys = break_pos.system_spec_index_;

  if (sys == VPOS) /* beginning of the book */
    return 0;
  if (system_specs_[sys].pscore_ && !break_pos.score_ender_)
    return sys; /* the score overflows the previous page */
  return sys + 1; /* this page starts with a new System_spec */
}

Page_breaking::Page_breaking (Paper_book *pb, Break_predicate is_break)
{
  book_ = pb;
  system_count_ = 0;
  ragged_ = to_boolean (pb->paper_->c_variable ("ragged-bottom"));
  ragged_last_ = to_boolean (pb->paper_->c_variable ("ragged-last-bottom"));
  page_top_space_ = robust_scm2double (pb->paper_->c_variable ("page-top-space"), 0);
  create_system_list ();
  find_chunks_and_breaks (is_break);
}

Page_breaking::~Page_breaking ()
{
}

bool
Page_breaking::ragged () const
{
  return ragged_;
}

bool
Page_breaking::ragged_last () const
{
  return ragged_last_;
}

Real
Page_breaking::page_top_space () const
{
  return page_top_space_;
}

vsize
Page_breaking::system_count () const
{
  return system_count_;
}

/* translate indices into breaks_ into start-end parameters for the line breaker */
void
Page_breaking::line_breaker_args (vsize sys,
				  Break_position const &start,
				  Break_position const &end,
				  vsize *line_breaker_start,
				  vsize *line_breaker_end)
{
  assert (system_specs_[sys].pscore_);
  assert (next_system (start) <= sys && sys <= end.system_spec_index_);

  if (start.system_spec_index_ == sys)
    *line_breaker_start = start.score_break_;
  else
    *line_breaker_start = 0;

  if (end.system_spec_index_ == sys)
    *line_breaker_end = end.score_break_;
  else
    *line_breaker_end = VPOS;
}

void
Page_breaking::break_into_pieces (vsize start_break, vsize end_break,
				  Line_division const &div)
{
  vector<Break_position> chunks = chunk_list (start_break, end_break);
  bool ignore_div = false;
  if (chunks.size () != div.size () + 1)
    {
      programming_error ("did not find a valid page breaking configuration");
      ignore_div = true;
    }

  for (vsize i = 0; i + 1 < chunks.size (); i++)
    {
      vsize sys = next_system (chunks[i]);
      if (system_specs_[sys].pscore_)
	{
	  vsize start;
	  vsize end;
	  line_breaker_args (sys, chunks[i], chunks[i+1], &start, &end);

	  vector<Column_x_positions> pos = ignore_div
	    ? line_breaking_[sys].best_solution (start, end)
	    : line_breaking_[sys].solve (start, end, div[i]);
	  system_specs_[sys].pscore_->root_system ()->break_into_pieces (pos);
	}
    }
}

SCM
Page_breaking::systems ()
{
  SCM ret = SCM_EOL;
  for (vsize sys = 0; sys < system_specs_.size (); sys++)
    {
      if (system_specs_[sys].pscore_)
	{
	  system_specs_[sys].pscore_->root_system ()
	    ->do_break_substitution_and_fixup_refpoints ();
	  SCM lines = system_specs_[sys].pscore_->root_system ()
	    ->get_broken_system_grobs ();
	  ret = scm_cons (lines, ret);
	}
      else
	{
	  Prob *pb = system_specs_[sys].prob_;
	  ret = scm_cons (scm_list_1 (pb->self_scm ()), ret);
	  pb->unprotect ();
	}
    }
  return scm_append (scm_reverse (ret));
}

Real
Page_breaking::page_height (int page_num, bool last) const
{
  bool last_part = ly_scm2bool (book_->paper_->c_variable ("is-last-bookpart"));
  SCM mod = scm_c_resolve_module ("scm page");
  SCM calc_height = scm_c_module_lookup (mod, "calc-printable-height");
  SCM make_page = scm_c_module_lookup (mod, "make-page");

  calc_height = scm_variable_ref (calc_height);
  make_page = scm_variable_ref (make_page);

  SCM page = scm_apply_0 (make_page, scm_list_n (
                  book_->self_scm (),
                  ly_symbol2scm ("page-number"), scm_from_int (page_num),
                  ly_symbol2scm ("is-last-bookpart"), scm_from_bool (last_part),
                  ly_symbol2scm ("is-bookpart-last-page"), scm_from_bool (last),
                  SCM_UNDEFINED));
  SCM height = scm_apply_1 (calc_height, page, SCM_EOL);
  return scm_to_double (height) - page_top_space_;
}

SCM
Page_breaking::breakpoint_property (vsize breakpoint, char const *str)
{
  Break_position const &pos = breaks_[breakpoint];

  if (pos.system_spec_index_ == VPOS)
    return SCM_EOL;
  if (system_specs_[pos.system_spec_index_].pscore_)
    return pos.col_->get_property (str);
  return system_specs_[pos.system_spec_index_].prob_->get_property (str);
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
  int first_page_number
    = robust_scm2int (book_->paper_->c_variable ("first-page-number"), 1);
  bool last_bookpart = ly_scm2bool (book_->paper_->c_variable ("is-last-bookpart"));
  SCM ret = SCM_EOL;
  SCM label_page_table = book_->top_paper ()->c_variable ("label-page-table");
  if (label_page_table == SCM_UNDEFINED)
    label_page_table = SCM_EOL;

  for (vsize i = 0; i < lines_per_page.size (); i++)
    {
      SCM page_num = scm_from_int (i + first_page_number);
      bool partbook_last_page = (i == lines_per_page.size () - 1);
      SCM rag = scm_from_bool (ragged () || ( partbook_last_page && ragged_last ()));
      SCM line_count = scm_from_int (lines_per_page[i]);
      SCM lines = scm_list_head (systems, line_count);
      SCM page = scm_apply_0 (make_page,
			      scm_list_n (book, lines, page_num, rag,
					  scm_from_bool (last_bookpart),
					  scm_from_bool (partbook_last_page),
					  SCM_UNDEFINED));
      /* collect labels */
      for (SCM l = lines ; scm_is_pair (l)  ; l = scm_cdr (l))
	{
	  SCM labels = SCM_EOL;
	  if (Grob * line = unsmob_grob (scm_car (l)))
	    {
	      System *system = dynamic_cast<System*> (line);
	      labels = system->get_property ("labels");
	    }
	  else if (Prob *prob = unsmob_prob (scm_car (l)))
	    labels = prob->get_property ("labels");

	  for (SCM lbls = labels ; scm_is_pair (lbls) ; lbls = scm_cdr (lbls))
	    label_page_table = scm_cons (scm_cons (scm_car (lbls), page_num),
					 label_page_table);
	}

      scm_apply_1 (page_stencil, page, SCM_EOL);
      ret = scm_cons (page, ret);
      systems = scm_list_tail (systems, line_count);
    }
  book_->top_paper ()->set_variable (ly_symbol2scm ("label-page-table"), label_page_table);
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
	    system_specs_.push_back (System_spec (ps));
	}
      else
        {
          Prob *pb = unsmob_prob (scm_car (s));
          assert (pb);

          pb->protect ();
          system_specs_.push_back (System_spec (pb));
        }
    }
}

void
Page_breaking::find_chunks_and_breaks (Break_predicate is_break)
{
  SCM force_sym = ly_symbol2scm ("force");

  chunks_.push_back (Break_position ());
  breaks_.push_back (Break_position ());

  for (vsize i = 0; i < system_specs_.size (); i++)
    {
      if (system_specs_[i].pscore_)
	{
	  vector<Grob*> cols
	    = system_specs_[i].pscore_->root_system ()->used_columns ();
	  vector<vsize> line_breaker_columns;
	  line_breaker_columns.push_back (0);

	  for (vsize j = 1; j < cols.size (); j++)
	    {
	      bool last = (j == cols.size () - 1);
	      bool break_point = is_break (cols[j]);
	      bool chunk_end = cols[j]->get_property ("page-break-permission") == force_sym;
	      Break_position cur_pos = Break_position (i,
						       line_breaker_columns.size (),
						       cols[j],
						       last);

	      if (break_point || (i == system_specs_.size () - 1 && last))
		breaks_.push_back (cur_pos);
	      if (chunk_end || last)
		chunks_.push_back (cur_pos);

	      if ((break_point || chunk_end) && !last)
		line_breaker_columns.push_back (j);
	    }
	  line_breaking_.push_back (Constrained_breaking (system_specs_[i].pscore_, line_breaker_columns));
	}
      else
	{
	  /* TODO: we want some way of applying Break_p to a prob? */
	  if (i == system_specs_.size () - 1)
	    breaks_.push_back (Break_position (i));

	  chunks_.push_back (Break_position (i));

	  /* FIXME: shouldn't we push a Null_breaker or similar dummy
	     class? --hwn */
	  line_breaking_.push_back (Constrained_breaking (NULL));
	}
    }
}

vector<Break_position>
Page_breaking::chunk_list (vsize start_index, vsize end_index)
{
  Break_position start = breaks_[start_index];
  Break_position end = breaks_[end_index];

  vsize i = 0;
  for (; i < chunks_.size () && chunks_[i] <= start; i++)
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
Page_breaking::system_count_bounds (vector<Break_position> const &chunks,
				    bool min)
{
  assert (chunks.size () >= 2);

  Line_division ret;
  ret.resize (chunks.size () - 1, 1);

  for (vsize i = 0; i + 1 < chunks.size (); i++)
    {
      vsize sys = next_system (chunks[i]);
      if (system_specs_[sys].pscore_)
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

void
Page_breaking::set_current_breakpoints (vsize start,
					vsize end,
					vsize system_count,
					Line_division lower_bound,
					Line_division upper_bound)
{
  system_count_ = system_count;
  current_chunks_ = chunk_list (start, end);
  current_start_breakpoint_ = start;
  current_end_breakpoint_ = end;
  clear_line_details_cache ();

  if (!lower_bound.size ())
    lower_bound = system_count_bounds (current_chunks_, true);
  if (!upper_bound.size ())
    upper_bound = system_count_bounds (current_chunks_, false);

  assert (lower_bound.size () == current_chunks_.size () - 1);
  assert (upper_bound.size () == current_chunks_.size () - 1);

  Line_division work_in_progress;
  current_configurations_.clear ();
  line_divisions_rec (system_count,
		      lower_bound,
		      upper_bound,
		      &work_in_progress);

  /* we only consider a constant number of configurations. Otherwise,
     this becomes slow when there are many small scores. The constant
     5 is somewhat arbitrary. */
  if (current_configurations_.size () > 5)
    {
      vector<pair<Real,vsize> > dems_and_indices;

      for (vsize i = 0; i < current_configurations_.size (); i++)
	{
	  cache_line_details (i);
	  Real dem = 0;
	  for (vsize j = 0; j < cached_line_details_.size (); j++)
	    dem += cached_line_details_[j].force_ * cached_line_details_[j].force_
	      + cached_line_details_[j].break_penalty_;

	  dems_and_indices.push_back (pair<Real,vsize> (dem, i));
	}
      vector_sort (dems_and_indices, less<pair<Real,vsize> > ());

      vector<Line_division> best_5_configurations;
      for (vsize i = 0; i < 5; i++)
	best_5_configurations.push_back (current_configurations_[dems_and_indices[i].second]);

      clear_line_details_cache ();
      current_configurations_ = best_5_configurations;
    }
}

void
Page_breaking::set_to_ideal_line_configuration (vsize start, vsize end)
{
  current_chunks_ = chunk_list (start, end);
  current_start_breakpoint_ = start;
  current_end_breakpoint_ = end;
  clear_line_details_cache ();
  system_count_ = 0;

  Line_division div;
  for (vsize i = 0; i+1 < current_chunks_.size (); i++)
    {
      vsize sys = next_system (current_chunks_[i]);
      if (system_specs_[sys].pscore_)
	{
	  line_breaker_args (sys, current_chunks_[i], current_chunks_[i+1], &start, &end);
	  div.push_back (line_breaking_[sys].best_solution (start, end).size ());
	}
      else
	div.push_back (1);

      system_count_ += div.back ();
    }
  current_configurations_.clear ();
  current_configurations_.push_back (div);
}

vsize
Page_breaking::current_configuration_count () const
{
  return current_configurations_.size ();
}

void
Page_breaking::cache_line_details (vsize configuration_index)
{
  if (cached_configuration_index_ != configuration_index)
    {
      cached_configuration_index_ = configuration_index;
      SCM padding_scm = book_->paper_->c_variable ("page-breaking-between-system-padding");
      if (!scm_is_number (padding_scm))
	padding_scm = book_->paper_->c_variable ("between-system-padding");
      Real padding = robust_scm2double (padding_scm, 0.0);

      Line_division &div = current_configurations_[configuration_index];
      uncompressed_line_details_.clear ();
      for (vsize i = 0; i + 1 < current_chunks_.size (); i++)
	{
	  vsize sys = next_system (current_chunks_[i]);
	  if (system_specs_[sys].pscore_)
	    {
	      vsize start;
	      vsize end;
	      line_breaker_args (sys, current_chunks_[i], current_chunks_[i+1], &start, &end);

	      vector<Line_details> details = line_breaking_[sys].line_details (start, end, div[i]);
	      uncompressed_line_details_.insert (uncompressed_line_details_.end (), details.begin (), details.end ());
	    }
	  else
	    {
	      assert (div[i] == 1);
	      uncompressed_line_details_.push_back (Line_details (system_specs_[sys].prob_));
	      uncompressed_line_details_.back ().padding_ =
                robust_scm2double (system_specs_[sys].prob_->get_property ("next-padding"),
                                   padding);
	    }
	}
      cached_line_details_ = compress_lines (uncompressed_line_details_);
    }
}

void
Page_breaking::clear_line_details_cache ()
{
  cached_configuration_index_ = VPOS;
  cached_line_details_.clear ();
  uncompressed_line_details_.clear ();
}

void
Page_breaking::line_divisions_rec (vsize system_count,
				   Line_division const &min_sys,
				   Line_division const &max_sys,
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
	current_configurations_.push_back (*cur_division);
      else
        line_divisions_rec (system_count - i, min_sys, max_sys, cur_division);
      cur_division->pop_back ();
    }
}

vsize
Page_breaking::min_page_count (vsize configuration, vsize first_page_num)
{
  vsize ret = 1;
  Real cur_rod_height = 0;
  Real cur_spring_height = 0;
  Real cur_page_height = page_height (first_page_num, false);

  cache_line_details (configuration);
  for (vsize i = 0; i < cached_line_details_.size (); i++)
    {
      Real ext_len = cached_line_details_[i].extent_.length ();
      Real next_rod_height = cur_rod_height + ext_len
	+ ((cur_rod_height > 0) ? cached_line_details_[i].padding_: 0);
      Real next_spring_height = cur_spring_height + cached_line_details_[i].space_;
      Real next_height = next_rod_height + (ragged () ? next_spring_height : 0);


      if ((next_height > cur_page_height && cur_rod_height > 0)
	  || (i > 0
	      && cached_line_details_[i-1].page_permission_ == ly_symbol2scm ("force")))
	{
	  cur_rod_height = ext_len;
	  cur_spring_height = cached_line_details_[i].space_;
	  cur_page_height = page_height (first_page_num + ret, false);
	  ret++;
	}
      else
	{
	  cur_rod_height = next_rod_height;
	  cur_spring_height = next_spring_height;
	}
    }

  /* there are two potential problems with the last page (because we didn't know
     it was the last page until after we managed to fit all the systems to it):
     - we are ragged-last but the last page has a compressed spring
     - the value returned by page_height (num, true) is smaller than the
       value returned by page_height (num, false) and it causes the page not to
       fit.

     In either case, we just need to add one more page. This is because the last
     line will always fit on the extra page and by adding one more page to the
     end, the previous page is no longer the last page, so our previous
     calculations that treated it as a non-last page were ok.
  */

  cur_page_height = page_height (first_page_num + ret - 1, true);
  Real cur_height = cur_rod_height + ((ragged_last () || ragged ()) ? cur_spring_height : 0);
  if (cur_height > cur_page_height
      /* don't increase the page count if the last page had only one system */
      && cur_rod_height > cached_line_details_.back ().extent_.length ())
    ret++;

  assert (ret <= cached_line_details_.size ());
  return ret;
}

// If systems_per_page is positive, we don't really try to space on N pages;
// we just put the requested number of systems on each page and penalize
// if the result doesn't have N pages.
Page_spacing_result
Page_breaking::space_systems_on_n_pages (vsize configuration, vsize n, vsize first_page_num,
					 int systems_per_page)
{
  Page_spacing_result ret;

  if (systems_per_page > 0)
    {
      Page_spacing_result ret = space_systems_with_fixed_number_per_page (configuration, first_page_num,
									  systems_per_page);
      ret.demerits_ += (ret.force_.size () == n) ? 0 : BAD_SPACING_PENALTY;
      return ret;
    }

  cache_line_details (configuration);
  bool valid_n = (n >= min_page_count (configuration, first_page_num)
		  && n <= cached_line_details_.size ());

  if (!valid_n)
    programming_error ("number of pages is out of bounds");

  if (n == 1 && valid_n)
    ret = space_systems_on_1_page (cached_line_details_,
				   page_height (first_page_num, is_last ()),
				   ragged () || (is_last () && ragged_last ()));
  else if (n == 2 && valid_n)
    ret = space_systems_on_2_pages (configuration, first_page_num);
  else
    {
      Page_spacer ps (cached_line_details_, first_page_num, this);
      ret = ps.solve (n);
    }

  return finalize_spacing_result (configuration, ret);
}

Real
Page_breaking::blank_page_penalty () const
{
  SCM penalty_sym;

  if (is_last ())
    penalty_sym = ly_symbol2scm ("blank-last-page-force");
  else if (ends_score ())
    penalty_sym = ly_symbol2scm ("blank-after-score-page-force");
  else
    penalty_sym = ly_symbol2scm ("blank-page-force");

  Break_position const &pos = breaks_[current_end_breakpoint_];
  if (Paper_score *ps = system_specs_[pos.system_spec_index_].pscore_)
    return robust_scm2double (ps->layout ()->lookup_variable (penalty_sym), 0.0);

  return robust_scm2double (book_->paper_->lookup_variable (penalty_sym), 0.0);
}

// If systems_per_page is positive, we don't really try to space on N
// or N+1 pages; see the comment to space_systems_on_n_pages.
Page_spacing_result
Page_breaking::space_systems_on_n_or_one_more_pages (vsize configuration, vsize n, vsize first_page_num,
						     int systems_per_page)
{
  Page_spacing_result n_res;
  Page_spacing_result m_res;

  if (systems_per_page > 0)
    {
      Page_spacing_result ret = space_systems_with_fixed_number_per_page (configuration, first_page_num,
									  systems_per_page);
      ret.demerits_ += (ret.force_.size () == n || ret.force_.size () == (n-1)) ? 0 : BAD_SPACING_PENALTY;
      return ret;
    }

  cache_line_details (configuration);
  vsize min_p_count = min_page_count (configuration, first_page_num);
  bool valid_n = n >= min_p_count || n <= cached_line_details_.size ();

  if (!valid_n)
    programming_error ("both page counts are out of bounds");

  if (n == 1 && valid_n)
    {
      bool rag = ragged () || (is_last () && ragged_last ());
      Real height = page_height (first_page_num, is_last ());

      if (1 >= min_p_count)
	n_res = space_systems_on_1_page (cached_line_details_, height, rag);
      if (1 < cached_line_details_.size ())
	m_res = space_systems_on_2_pages (configuration, first_page_num);
    }
  else
    {
      Page_spacer ps (cached_line_details_, first_page_num, this);
      
      if (n >= min_p_count || !valid_n)
	n_res = ps.solve (n);
      if (n < cached_line_details_.size () || !valid_n)
	m_res = ps.solve (n+1);
    }

  m_res = finalize_spacing_result (configuration, m_res);
  n_res = finalize_spacing_result (configuration, n_res);

  Real penalty = blank_page_penalty ();
  n_res.demerits_ += penalty;

  if (n_res.force_.size ())
    n_res.force_.back () += penalty;

  return (m_res.demerits_ < n_res.demerits_) ? m_res : n_res;
}

Page_spacing_result
Page_breaking::space_systems_on_best_pages (vsize configuration, vsize first_page_num)
{
  vsize min_p_count = min_page_count (configuration, first_page_num);
  Real odd_pages_penalty = blank_page_penalty ();

  cache_line_details (configuration);
  Page_spacer ps (cached_line_details_, first_page_num, this);
  Page_spacing_result best = ps.solve (min_p_count);
  best.force_.back () += (min_p_count % 2) ? odd_pages_penalty : 0;
  best.demerits_ += (min_p_count % 2) ? odd_pages_penalty : 0;

  for (vsize i = min_p_count+1; i <= cached_line_details_.size (); i++)
    {
      Page_spacing_result cur = ps.solve (i);
      cur.demerits_ += (i % 2) ? odd_pages_penalty : 0;
      if (cur.demerits_ < best.demerits_)
	best = cur;
    }

  return finalize_spacing_result (configuration, best);
}

Page_spacing_result
Page_breaking::space_systems_with_fixed_number_per_page (vsize configuration,
							 vsize first_page_num,
							 int systems_per_page)
{
  Page_spacing_result res;
  Page_spacing space (page_height (first_page_num, false), page_top_space_);
  vsize line = 0;
  vsize page = 0;
  vsize page_first_line = 0;

  cache_line_details (configuration);
  while (line < cached_line_details_.size ())
    {
      page++;
      space.clear ();
      space.resize (page_height (first_page_num + page, false));

      int system_count_on_this_page = 0;
      while (system_count_on_this_page < systems_per_page
	     && line < cached_line_details_.size ())
	{
	  Line_details const &cur_line = cached_line_details_[line];
	  space.append_system (cur_line);
	  system_count_on_this_page += cur_line.compressed_nontitle_lines_count_;
	  line++;

	  if (cur_line.page_permission_ == ly_symbol2scm ("force"))
	    break;
	}

      res.systems_per_page_.push_back (line - page_first_line);
      res.force_.push_back (space.force_);
      res.penalty_ += cached_line_details_[line-1].page_penalty_;
      page_first_line = line;
    }

  /* Recalculate forces for the last page because we know now that is
     was really the last page. */
  space.resize (page_height (first_page_num + page, true));
  res.force_.back () = space.force_;
  return finalize_spacing_result (configuration, res);
}

Page_spacing_result
Page_breaking::pack_systems_on_least_pages (vsize configuration, vsize first_page_num)
{
  Page_spacing_result res;
  vsize page = 0;
  vsize page_first_line = 0;
  Page_spacing space (page_height (first_page_num, false), page_top_space_);

  cache_line_details (configuration);
  for (vsize line = 0; line < cached_line_details_.size (); line++)
    {
      Real prev_force = space.force_;
      space.append_system (cached_line_details_[line]);
      if ((line > page_first_line)
	  && (isinf (space.force_)
	      || ((line > 0)
                  && (cached_line_details_[line-1].page_permission_ == ly_symbol2scm ("force")))))
	{
	  res.systems_per_page_.push_back (line - page_first_line);
	  res.force_.push_back (prev_force);
          res.penalty_ += cached_line_details_[line-1].page_penalty_;
	  page++;
	  space.resize (page_height (first_page_num + page, false));
	  space.clear ();
          space.append_system (cached_line_details_[line]);
	  page_first_line = line;
	}

      if (line == cached_line_details_.size () - 1)
	{
	  /* This is the last line */
	  /* When the last page height was computed, we did not know yet that it
	   * was the last one. If the systems put on it don't fit anymore, the last
	   * system is moved to a new page */
	  space.resize (page_height (first_page_num + page, true));
	  if ((line > page_first_line) && (isinf (space.force_)))
	    {
	      res.systems_per_page_.push_back (line - page_first_line);
	      res.force_.push_back (prev_force);
	      /* the last page containing the last line */
	      space.resize (page_height (first_page_num + page + 1, true));
	      space.clear ();
	      space.append_system (cached_line_details_[line]);
	      res.systems_per_page_.push_back (1);
	      res.force_.push_back (space.force_);
              res.penalty_ += cached_line_details_[line-1].page_penalty_;
              res.penalty_ += cached_line_details_[line].page_penalty_;
	    }
	  else
	    {
	      res.systems_per_page_.push_back (line + 1 - page_first_line);
	      res.force_.push_back (space.force_);
              res.penalty_ += cached_line_details_[line].page_penalty_;
	    }
	}
    }
  return finalize_spacing_result (configuration, res);
}

/* Calculate demerits and fix res.systems_per_page_ so that
   it refers to the original line numbers, not the ones given by compress_lines (). */
Page_spacing_result
Page_breaking::finalize_spacing_result (vsize configuration, Page_spacing_result res)
{
  if (res.force_.empty ())
    return res;

  cache_line_details (configuration);
  res.systems_per_page_ = uncompress_solution (res.systems_per_page_, cached_line_details_);

  Real line_force = 0;
  Real line_penalty = 0;
  Real page_force = 0;
  Real page_weighting = robust_scm2double (book_->paper_->c_variable ("page-spacing-weight"), 10);

  for (vsize i = 0; i < uncompressed_line_details_.size (); i++)
    {
      line_force += uncompressed_line_details_[i].force_ * uncompressed_line_details_[i].force_;
      line_penalty += uncompressed_line_details_[i].break_penalty_;
    }

  for (vsize i = 0; i < res.force_.size (); i++)
    {
      Real f = res.force_[i];
      if (isinf (f) && res.systems_per_page_[i] == 1)
	f = 20000;

      page_force += f * f;
    }

  /* for a while we tried averaging page and line forces across pages instead
     of summing them, but it caused a problem: if there is a single page
     with a very bad page force (for example because of a forced page break),
     the page breaker will put in a _lot_ of pages so that the bad force
     becomes averaged out over many pages. */
  res.demerits_ = line_force + line_penalty + (page_force + res.penalty_) * page_weighting;
  return res;

}

/* the cases for page_count = 1 or 2 can be done in O (n) time. Since they
   are by far the most common cases, we have special functions for them.

   space_systems_on_1_page has a different calling convention than most of the
   space_systems functions. This is because space_systems_on_1_page is (unlike
   the other space_systems functions) sometimes called on subsets of a full
   configuration. */
Page_spacing_result
Page_breaking::space_systems_on_1_page (vector<Line_details> const &lines, Real page_height, bool ragged)
{
  Page_spacing space (page_height, page_top_space_);
  Page_spacing_result ret;

  for (vsize i = 0; i < lines.size (); i++)
    space.append_system (lines[i]);

  ret.systems_per_page_.push_back (lines.size ());
  ret.force_.push_back (ragged ? min (space.force_, 0.0) : space.force_);
  ret.penalty_ = lines.back ().page_penalty_ + lines.back ().turn_penalty_;

  /* don't do finalize_spacing_result () because we are only an internal function */
  return ret;
}

Page_spacing_result
Page_breaking::space_systems_on_2_pages (vsize configuration, vsize first_page_num)
{
  Real page1_height = page_height (first_page_num, false);
  Real page2_height = page_height (first_page_num + 1, is_last ());
  bool ragged1 = ragged ();
  bool ragged2 = ragged () || (is_last () && ragged_last ());

  /* if there is a forced break, this reduces to 2 1-page problems */
  cache_line_details (configuration);
  for (vsize i = 0; i + 1 < cached_line_details_.size (); i++)
    if (cached_line_details_[i].page_permission_ == ly_symbol2scm ("force"))
      {
	vector<Line_details> lines1 (cached_line_details_.begin (), cached_line_details_.begin () + i + 1);
	vector<Line_details> lines2 (cached_line_details_.begin () + i + 1, cached_line_details_.end ());
	Page_spacing_result p1 = space_systems_on_1_page (lines1, page1_height, ragged1);
	Page_spacing_result p2 = space_systems_on_1_page (lines2, page2_height, ragged2);

	p1.systems_per_page_.push_back (p2.systems_per_page_[0]);
	p1.force_.push_back (p2.force_[0]);
	p1.penalty_ += p2.penalty_ - cached_line_details_[i].turn_penalty_;
	return p1;
      }

  vector<Real> page1_force;
  vector<Real> page2_force;
  Page_spacing page1 (page1_height, page_top_space_);
  Page_spacing page2 (page2_height, page_top_space_);

  page1_force.resize (cached_line_details_.size () - 1, infinity_f);
  page2_force.resize (cached_line_details_.size () - 1, infinity_f);

  /* find the page 1 and page 2 forces for each page-breaking position */
  for (vsize i = 0; i < page1_force.size (); i++)
    {
      page1.append_system (cached_line_details_[i]);
      page2.prepend_system (cached_line_details_[cached_line_details_.size () - 1 - i]);
      page1_force[i] = (ragged1 && page1.force_ < 0 && i > 0) ? infinity_f : page1.force_;

      if (ragged2)
	page2_force[page2_force.size () - 1 - i] =
	  (page2.force_ < 0 && i + 1 < page1_force.size ()) ? infinity_f : 0;
      else
	page2_force[page2_force.size () - 1 - i] = page2.force_;
    }

  /* find the position that minimises the sum of the page forces */
  vsize best_sys_count = 1;
  Real best_demerits = infinity_f;
  for (vsize i = 0; i < page1_force.size (); i++)
    {
      Real f = page1_force[i] * page1_force[i] + page2_force[i] * page2_force[i];
      Real uneven = 2 * (page1_force[i] - page2_force[i]);
      Real dem = uneven * uneven + f
	+ cached_line_details_[i+1].page_penalty_
	+ cached_line_details_.back ().page_penalty_ + cached_line_details_.back ().turn_penalty_;
      if (dem < best_demerits)
	{
	  best_demerits = dem;
	  best_sys_count = i+1;
	}
    }

  Page_spacing_result ret;
  ret.systems_per_page_.push_back (best_sys_count);
  ret.systems_per_page_.push_back (cached_line_details_.size () - best_sys_count);
  ret.force_.push_back (page1_force[best_sys_count-1]);
  ret.force_.push_back (page2_force[best_sys_count-1]);
  ret.penalty_ = cached_line_details_[best_sys_count-1].page_penalty_
    + cached_line_details_.back ().page_penalty_
    + cached_line_details_.back ().turn_penalty_;

  /* don't do finalize_spacing_result () because we are only an internal function */
  return ret;
}

bool
Page_breaking::all_lines_stretched (vsize configuration)
{
  cache_line_details (configuration);
  for (vsize i = 0; i < cached_line_details_.size (); i++)
    if (cached_line_details_[i].force_ < 0)
      return false;

  return true;
}

Page_breaking::Line_division
Page_breaking::current_configuration (vsize configuration_index) const
{
  return current_configurations_[configuration_index];
}

bool
Page_breaking::is_last () const
{
  return current_end_breakpoint_ == last_break_position ();
}

bool
Page_breaking::ends_score () const
{
  return breaks_[current_end_breakpoint_].score_ender_;
}

vsize
Page_breaking::last_break_position () const
{
  return breaks_.size () - 1;  
}
