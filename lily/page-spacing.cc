/*
  page-spacing.cc - implement routines for spacing
  systems vertically on pages

  source file of the GNU LilyPond music typesetter

  (c) 2006--2007 Joe Neeman <joeneeman@gmail.com>
*/

#include "page-spacing.hh"

#include "matrix.hh"
#include "page-breaking.hh"
#include "warn.hh"

/* In order to prevent possible division by zero, we require every line
   to have a spring of non-zero length. */
Real
line_space (const Line_details &line)
{
  return max (0.1, line.space_);
}

void
Page_spacing::calc_force ()
{
  if (rod_height_ + last_line_.bottom_padding_ >= page_height_ || !inverse_spring_k_)
    force_ = infinity_f;
  else
    force_ = (page_height_ - rod_height_ - last_line_.bottom_padding_ - spring_len_) / inverse_spring_k_;
}

void
Page_spacing::append_system (const Line_details &line)
{
  rod_height_ += last_line_.padding_;

  rod_height_ += line.extent_.length ();
  spring_len_ += line_space (line);
  inverse_spring_k_ += max (0.1, line.inverse_hooke_);

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
  spring_len_ += line_space (line);
  inverse_spring_k_ += max (0.1, line.inverse_hooke_);

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
  ragged_last_ = breaker->last () && breaker->ragged_last ();
}

Spacing_result
Page_spacer::solve (vsize page_count)
{
  if (page_count > max_page_count_)
    resize (page_count);

  Spacing_result ret;
  ret.force_.resize (page_count);
  ret.systems_per_page_.resize (page_count);

  vsize system = lines_.size () - 1;
  vsize tack_onto_the_end = 0;

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
	  tack_onto_the_end = system - i;
	  system = i;
	}
      else
	return Spacing_result (); /* couldn't salvage it -- probably going to crash */
    }

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
	ret.systems_per_page_[p] = system - ps.prev_ + tack_onto_the_end;
      system = ps.prev_;
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

bool
Page_spacer::calc_subproblem (vsize page, vsize line)
{
  bool last = line == lines_.size () - 1;
  Page_spacing space (breaker_->page_height (page + first_page_num_, last));
  Page_spacing_node &cur = state_.at (line, page);
  bool ragged = ragged_ || (ragged_last_ && last);

  for (vsize page_start = line+1; page_start > page && page_start--;)
    {
      Page_spacing_node const *prev = page > 0 ? &state_.at (page_start-1, page-1) : 0;

      space.prepend_system (lines_[page_start]);
      if (page_start < line && (isinf (space.force_) || (space.force_ < 0 && ragged)))
	break;

      if (page > 0 || page_start == 0)
	{
	  if (line == lines_.size () - 1 && ragged_last_ && space.force_ > 0)
	    space.force_ = 0;

	  /* we may have to deal with single lines that are taller than a page */
	  if (isinf (space.force_) && page_start == line)
	    space.force_ = -200000;

	  Real dem = fabs (space.force_) + (prev ? prev->demerits_ : 0);
	  Real penalty = 0;
	  if (page_start > 0)
	    penalty = lines_[page_start-1].page_penalty_
	      + (page % 2 == 0) ? lines_[page_start-1].turn_penalty_ : 0;

	  dem += penalty;
	  if (dem < cur.demerits_ || page_start == line)
	    {
	      cur.demerits_ = dem;
	      cur.force_ = space.force_;
	      cur.penalty_ = penalty + (prev ? prev->penalty_ : 0);
	      cur.prev_ = page_start - 1;
	    }
	}

      if (page_start > 0
	  && lines_[page_start-1].page_permission_ == ly_symbol2scm ("force"))
	break;
    }
  return !isinf (cur.demerits_);
}

