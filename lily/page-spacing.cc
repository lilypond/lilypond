/*
  page-spacing.cc - implement routines for spacing
  systems vertically on pages

  source file of the GNU LilyPond music typesetter

  (c) 2006--2009 Joe Neeman <joeneeman@gmail.com>
*/

#include "page-spacing.hh"

#include "matrix.hh"
#include "page-breaking.hh"
#include "warn.hh"

void
Page_spacing::calc_force ()
{
  /* If the first system is a title, we add back in the page-top-space. */
  Real height = first_line_.title_ ? page_height_ + page_top_space_ : page_height_;

  if (rod_height_ + last_line_.bottom_padding_ >= height)
    force_ = infinity_f;
  else
    force_ = (height - rod_height_ - last_line_.bottom_padding_ - spring_len_)
      / max (0.1, inverse_spring_k_);
}

void
Page_spacing::resize (Real new_height)
{
  page_height_ = new_height;
  calc_force ();
}

void
Page_spacing::append_system (const Line_details &line)
{
  if (!rod_height_)
    first_line_ = line;

  rod_height_ += last_line_.padding_;

  rod_height_ += line.extent_.length ();
  spring_len_ += line.space_;
  inverse_spring_k_ += line.inverse_hooke_;

  last_line_ = line;

  calc_force ();
}

void
Page_spacing::prepend_system (const Line_details &line)
{
  if (rod_height_)
    rod_height_ += line.padding_;
  else
    last_line_ = line;

  rod_height_ += line.extent_.length ();
  spring_len_ += line.space_;
  inverse_spring_k_ += line.inverse_hooke_;

  first_line_ = line;

  calc_force ();
}

void
Page_spacing::clear ()
{
  force_ = rod_height_ = spring_len_ = 0;
  inverse_spring_k_ = 0;
}


Page_spacer::Page_spacer (vector<Line_details> const &lines, vsize first_page_num, Page_breaking const *breaker)
  : lines_ (lines)
{
  first_page_num_ = first_page_num;
  breaker_ = breaker;
  max_page_count_ = 0;
  ragged_ = breaker->ragged ();
  ragged_last_ = breaker->is_last () && breaker->ragged_last ();
}

Page_spacing_result
Page_spacer::solve (vsize page_count)
{
  if (page_count > max_page_count_)
    resize (page_count);

  Page_spacing_result ret;

  vsize system = lines_.size () - 1;
  vsize extra_systems = 0;
  vsize extra_pages = 0;

  if (isinf (state_.at (system, page_count-1).demerits_))
    {
      programming_error ("tried to space systems on a bad number of pages");
      /* Usually, this means that we tried to cram too many systems into
	 to few pages. To avoid crashing, we look for the largest number of
	 systems that we can fit properly onto the right number of pages.
	 All the systems that don't fit get tacked onto the last page.
      */
      vsize i;
      for (i = system; isinf (state_.at (i, page_count-1).demerits_) && i; i--)
	;

      if (i)
	{
	  extra_systems = system - i;
	  system = i;
	}
      else
	{
	  /* try chopping off pages from the end */
	  vsize j;
	  for (j = page_count; j && isinf (state_.at (system, j-1).demerits_); j--)
	    ;

	  if (j)
	    {
	      extra_pages = page_count - j;
	      page_count = j;
	    }
	  else
	    return Page_spacing_result (); /* couldn't salvage it -- probably going to crash */
	}
    }

  ret.force_.resize (page_count);
  ret.systems_per_page_.resize (page_count);
  ret.penalty_ = state_.at (system, page_count-1).penalty_
    + lines_.back ().page_penalty_ + lines_.back ().turn_penalty_;

  ret.demerits_ = 0;
  for (vsize p = page_count; p--;)
    {
      assert (system != VPOS);

      Page_spacing_node const &ps = state_.at (system, p);
      ret.force_[p] = ps.force_;
      ret.demerits_ += ps.force_ * ps.force_;
      if (p == 0)
	ret.systems_per_page_[p] = system + 1;
      else
	ret.systems_per_page_[p] = system - ps.prev_;
      system = ps.prev_;
    }

  if (extra_systems)
    {
      ret.systems_per_page_.back () += extra_systems;
      ret.demerits_ += BAD_SPACING_PENALTY;
    }
  if (extra_pages)
    {
      ret.force_.insert (ret.force_.end (), extra_pages, BAD_SPACING_PENALTY);
      ret.systems_per_page_.insert (ret.systems_per_page_.end (), extra_pages, 0);
      ret.demerits_ += BAD_SPACING_PENALTY;
    }


  ret.demerits_ += ret.penalty_;
  return ret;
}

void
Page_spacer::resize (vsize page_count)
{
  assert (page_count > 0);

  if (max_page_count_ >= page_count)
    return;

  state_.resize (lines_.size (), page_count, Page_spacing_node ());
  for (vsize page = max_page_count_; page < page_count; page++)
    for (vsize line = page; line < lines_.size (); line++)
      if (!calc_subproblem (page, line))
	break;

  max_page_count_ = page_count;
}

// Carries out one step in the dynamic programming algorithm for putting systems
// on a fixed number of pages. One call to this routine calculates the best
// configuration for putting lines 0 through LINE-1 on PAGE+1 pages, provided that
// we have previously called calc_subproblem(page-1, k) for every k < LINE.
//
// This algorithm is similar to the constrained-breaking algorithm.
bool
Page_spacer::calc_subproblem (vsize page, vsize line)
{
  bool last = line == lines_.size () - 1;
  Page_spacing space (breaker_->page_height (page + first_page_num_, last),
		      breaker_->page_top_space ());
  Page_spacing_node &cur = state_.at (line, page);
  bool ragged = ragged_ || (ragged_last_ && last);
  int line_count = 0;

  for (vsize page_start = line+1; page_start > page && page_start--;)
    {
      Page_spacing_node const *prev = page > 0 ? &state_.at (page_start-1, page-1) : 0;

      space.prepend_system (lines_[page_start]);

      // This 'if' statement is a little hard to parse. It won't consider this configuration
      // if it is overfull unless the current configuration is the first one with this start
      // point. We also make an exception (and consider this configuration) if the previous
      // configuration we tried had fewer lines than min-systems-per-page.
      if (!breaker_->too_few_lines (line_count)
	  && page_start < line
	  && (isinf (space.force_) || (space.force_ < 0 && ragged)))
	break;

      line_count += lines_[page_start].compressed_nontitle_lines_count_;
      if (page > 0 || page_start == 0)
	{
	  // If the last page is ragged, set its force to zero. This way, we will leave
	  // the last page half-empty rather than trying to balance things out
	  // (which only makes sense in non-ragged situations).
	  if (line == lines_.size () - 1 && ragged && last && space.force_ > 0)
	    space.force_ = 0;

	  Real demerits = space.force_ * space.force_;
	  /* If a single line is taller than a page, we need to consider it as
	     a possible solution (but we give it a very bad score). */
	  if (isinf (space.force_) && page_start == line)
	    demerits = BAD_SPACING_PENALTY;
	  demerits += (prev ? prev->demerits_ : 0);

	  Real penalty = breaker_->line_count_penalty (line_count);
	  if (page_start > 0)
	    penalty += lines_[page_start-1].page_penalty_
	      + (page % 2 == 0) ? lines_[page_start-1].turn_penalty_ : 0;

	  demerits += penalty;
	  if (demerits < cur.demerits_ || page_start == line)
	    {
	      cur.demerits_ = demerits;
	      cur.force_ = space.force_;
	      cur.penalty_ = penalty + (prev ? prev->penalty_ : 0);
	      cur.system_count_status_ = breaker_->line_count_status (line_count)
		| (prev ? prev->system_count_status_ : 0);
	      cur.prev_ = page_start - 1;
	    }
	}

      if (page_start > 0
	  && lines_[page_start-1].page_permission_ == ly_symbol2scm ("force"))
	break;
    }
  return !isinf (cur.demerits_);
}

