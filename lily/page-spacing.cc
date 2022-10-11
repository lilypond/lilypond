/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Joe Neeman <joeneeman@gmail.com>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "page-spacing.hh"

#include "international.hh"
#include "page-breaking.hh"
#include "warn.hh"

#include <algorithm>
#include <vector>

using std::vector;

void
Page_spacing::calc_force ()
{
  Real height = page_height_
                - breaker_->min_whitespace_at_top_of_page (first_line_)
                - breaker_->min_whitespace_at_bottom_of_page (last_line_);

  if (rod_height_ + last_line_.bottom_padding_ >= height)
    force_ = -infinity_f;
  else
    force_ = (height - rod_height_ - last_line_.bottom_padding_ - spring_len_)
             / std::max (0.1, inverse_spring_k_);
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
  if (rod_height_)
    {
      rod_height_ += line.tallness_;
      spring_len_ += last_line_.spring_length (line);
    }
  else
    {
      rod_height_ += line.full_height ();
      first_line_ = line;
    }

  rod_height_ += account_for_footnotes (line);
  inverse_spring_k_ += line.inverse_hooke_;

  last_line_ = line;

  calc_force ();
}

Real
Page_spacing::account_for_footnotes (Line_details const &line)
{
  Real footnote_height = 0.0;
  Real in_note_height = 0.0;
  bool has_in_notes = false;
  for (vsize i = 0; i < line.in_note_heights_.size (); i++)
    {
      in_note_height += (has_in_notes ? 0.0 : breaker_->in_note_padding ());
      has_in_notes = true;
      in_note_height += line.in_note_heights_[i];
    }

  for (vsize i = 0; i < line.footnote_heights_.size (); i++)
    {
      footnote_height
        += (has_footnotes_ ? 0.0
                           : (breaker_->footnote_separator_stencil_height ()
                              + breaker_->footnote_padding ()
                              + breaker_->footnote_number_raise ()));

      has_footnotes_ = true;
      footnote_height += line.footnote_heights_[i];
      footnote_height += breaker_->footnote_padding ();
    }

  return (in_note_height - (has_in_notes ? breaker_->in_note_padding () : 0.0))
         + (footnote_height
            + (has_footnotes_ ? -breaker_->footnote_padding ()
                                  + breaker_->footnote_footer_padding ()
                              : 0.0));
}

void
Page_spacing::prepend_system (const Line_details &line)
{
  if (rod_height_)
    spring_len_ += line.spring_length (first_line_);
  else
    last_line_ = line;

  rod_height_ -= first_line_.full_height ();
  rod_height_ += first_line_.tallness_;
  rod_height_ += line.full_height ();
  rod_height_ += account_for_footnotes (line);
  inverse_spring_k_ += line.inverse_hooke_;

  first_line_ = line;

  calc_force ();
}

void
Page_spacing::clear ()
{
  force_ = rod_height_ = spring_len_ = 0;
  inverse_spring_k_ = 0;
  has_footnotes_ = false;
}

Page_spacer::Page_spacer (vector<Line_details> const &lines, int first_page_num,
                          Page_breaking const *breaker)
  : lines_ (lines)
{
  first_page_num_ = first_page_num;
  breaker_ = breaker;
  max_page_count_ = 0;
  ragged_ = breaker->ragged ();
  ragged_last_ = breaker->is_last () && breaker->ragged_last ();
}

Page_spacing_result
Page_spacer::solve ()
{
  if (simple_state_.empty ())
    {
      simple_state_.resize (lines_.size ());
      for (vsize i = 0; i < lines_.size (); ++i)
        calc_subproblem (VPOS, i);
    }

  Page_spacing_result ret;
  if (simple_state_.empty ())
    return ret;

  ret.penalty_ = simple_state_.back ().penalty_ + lines_.back ().page_penalty_
                 + lines_.back ().turn_penalty_;
  ret.system_count_status_ = simple_state_.back ().system_count_status_;

  vsize system = lines_.size () - 1;
  while (system != VPOS)
    {
      Page_spacing_node const &cur = simple_state_[system];
      vsize system_count
        = (cur.prev_ == VPOS) ? system + 1 : system - cur.prev_;

      ret.force_.push_back (cur.force_);
      ret.systems_per_page_.push_back (system_count);
      ret.demerits_ += cur.force_ * cur.force_;
      system = cur.prev_;
    }

  std::reverse (ret.force_.begin (), ret.force_.end ());
  std::reverse (ret.systems_per_page_.begin (), ret.systems_per_page_.end ());
  return ret;
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

  if (std::isinf (state_.at (system, page_count - 1).demerits_))
    {
      warning (_ ("tried to space systems on a bad number of pages"));
      /* Usually, this means that we tried to cram too many systems into
         to few pages (can happen if the requested page-count is unreasonable).
         To avoid crashing, we look for the largest number of systems that
         we can fit properly onto the right number of pages.  All the systems
         that don't fit get tacked onto the last page.
      */
      vsize i;
      for (i = system;
           std::isinf (state_.at (i, page_count - 1).demerits_) && i; i--)
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
          for (j = page_count;
               j && std::isinf (state_.at (system, j - 1).demerits_); j--)
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
  ret.system_count_status_
    = state_.at (system, page_count - 1).system_count_status_;
  ret.penalty_ = state_.at (system, page_count - 1).penalty_
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
      ret.force_.back () = BAD_SPACING_PENALTY;
    }
  if (extra_pages)
    {
      ret.force_.insert (ret.force_.end (), extra_pages, BAD_SPACING_PENALTY);
      ret.systems_per_page_.insert (ret.systems_per_page_.end (), extra_pages,
                                    0);
    }

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
//
// If page == VPOS, we act on simple_state_ instead of state_.  This is useful if
// we don't want to constrain the number of pages that the solution has.  In this
// case, the algorithm looks more like the page-turn-page-breaking algorithm.  But
// the subproblems look similar for both, so we reuse this method.
bool
Page_spacer::calc_subproblem (vsize page, vsize line)
{
  bool last = line == lines_.size () - 1;

  // Note: if page == VPOS then we don't actually know yet which page number we're
  // working on, so we have to recalculate the page height in the loop.  Therefore
  // our early-exit condition from the loop depends on paper_height rather than
  // page_height (ie. we break only if we would overfill a page without margins
  // or headers/footers).  Otherwise, the algorithm would not be optimal:
  // if our page has a very large header then perhaps
  // we should look ahead a few systems in order to find the best solution.  A
  // good example of this is input/regression/page-spacing-tall-headfoot.ly
  vsize page_num = page == VPOS ? 0 : page;
  Real paper_height = breaker_->paper_height ();
  Page_spacing space (
    breaker_->page_height (first_page_num_ + static_cast<int> (page), last),
    breaker_);
  Page_spacing_node &cur
    = page == VPOS ? simple_state_[line] : state_.at (line, page);
  bool ragged = ragged_ || (ragged_last_ && last);
  int line_count = 0;

  for (vsize page_start = line + 1; page_start > page_num && page_start--;)
    {
      Page_spacing_node const *prev = 0;

      if (page == VPOS)
        {
          if (page_start > 0)
            {
              prev = &simple_state_[page_start - 1];
              space.resize (breaker_->page_height (prev->page_ + 1, last));
            }
          else
            space.resize (breaker_->page_height (first_page_num_, last));
        }
      else if (page > 0)
        prev = &state_.at (page_start - 1, page - 1);

      space.prepend_system (lines_[page_start]);

      bool overfull
        = (space.rod_height_ > paper_height
           || (ragged_
               && (space.rod_height_ + space.spring_len_ > paper_height)));
      // This 'if' statement is a little hard to parse. It won't consider this configuration
      // if it is overfull unless the current configuration is the first one with this start
      // point. We also make an exception (and consider this configuration) if the previous
      // configuration we tried had fewer lines than min-systems-per-page.
      if (!breaker_->too_few_lines (line_count) && page_start < line
          && overfull)
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

          // Clamp the demerits at BAD_SPACING_PENALTY, even if the page
          // is overfull.  This ensures that TERRIBLE_SPACING_PENALTY takes
          // precedence over overfull pages.
          demerits = std::min (demerits, BAD_SPACING_PENALTY);
          demerits += (prev ? prev->demerits_ : 0);

          Real penalty = breaker_->line_count_penalty (line_count);
          if (page_start > 0)
            penalty
              += lines_[page_start - 1].page_penalty_
                 + ((page % 2 == 0) ? lines_[page_start - 1].turn_penalty_ : 0);

          /* Deal with widow/orphan lines */
          /* Last line of paragraph is first line on the new page */
          if ((page_start > 0) && (page_start < lines_.size ())
              && (lines_[page_start].last_markup_line_))
            penalty += breaker_->orphan_penalty ();
          /* First line of paragraph is last line on the previous page */
          if ((page_start > 0) && (page_start < lines_.size ())
              && (lines_[page_start - 1].first_markup_line_))
            penalty += breaker_->orphan_penalty ();

          demerits += penalty;
          if (demerits < cur.demerits_ || page_start == line)
            {
              cur.demerits_ = demerits;
              cur.force_ = space.force_;
              cur.penalty_ = penalty + (prev ? prev->penalty_ : 0);
              cur.system_count_status_
                = breaker_->line_count_status (line_count)
                  | (prev ? prev->system_count_status_ : 0);
              cur.prev_ = page_start - 1;
              cur.page_ = prev ? prev->page_ + 1 : first_page_num_;
            }
        }

      if (page_start > 0
          && scm_is_eq (lines_[page_start - 1].page_permission_,
                        ly_symbol2scm ("force")))
        break;
    }
  return !std::isinf (cur.demerits_);
}
