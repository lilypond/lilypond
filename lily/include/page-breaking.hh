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

#ifndef PAGE_BREAKING_HH
#define PAGE_BREAKING_HH

#include "constrained-breaking.hh"
#include "page-spacing.hh"

#include <vector>

/* Either a paper-score, markup or header.
 */
struct System_spec
{
  System_spec (Paper_score *ps)
  {
    pscore_ = ps;
    prob_ = NULL;
  }

  System_spec (Prob *pb)
  {
    prob_ = pb;
    pscore_ = NULL;
  }

  /* only used as a dummy value in case the book is empty. */
  System_spec ()
  {
    pscore_ = NULL;
    prob_ = NULL;
  }

  Paper_score *pscore_;
  Prob *prob_;
};

/*
  indicates a break point in a page spacing problem. If there are N
  lines/systems, there are N+1 Break_positions around those lines.
*/
struct Break_position
{
  /*
    index into system_specs_, if this is VPOS, the Break_position represents the
    start of the book.
   */
  vsize system_spec_index_;

  /* if system_spec_index_ indexes a score, then we start at the score_brk_'th
     possible page-break in the score */
  vsize score_break_;

  /* if system_spec_index_ indexes a score, this points to the broken column */
  Grob *col_;
  bool score_ender_;

  /* if non-zero, this is the (fixed, uncompressed) number of lines between
     this Break_position and the previous. */
  vsize forced_line_count_;

  Break_position (vsize s = VPOS, vsize brk = VPOS, Grob *g = NULL,
                  bool end = false)
  {
    system_spec_index_ = s;
    score_break_ = brk;
    col_ = g;
    score_ender_ = end;
    forced_line_count_ = 0;
  }

  /*
    lexicographic in (system_spec_index_, score_break_)
   */
  bool operator<(const Break_position &other) const
  {
    return (system_spec_index_ == VPOS && other.system_spec_index_ != VPOS)
           || (system_spec_index_ < other.system_spec_index_)
           || (system_spec_index_ == other.system_spec_index_
               && score_break_ < other.score_break_);
  }

  bool operator<= (const Break_position &other) const
  {
    return (system_spec_index_ == VPOS)
           || (system_spec_index_ < other.system_spec_index_
               && other.system_spec_index_ != VPOS)
           || (system_spec_index_ == other.system_spec_index_
               && score_break_ <= other.score_break_);
  }
};

/*
  A page breaking problem. Subclasses represent different strategies.

  Read the large comment at the top of page-breaking.cc for context.
*/
class Page_breaking
{
public:
  typedef bool (*Break_predicate) (Grob *);
  typedef bool (*Prob_break_predicate) (Prob *);
  typedef std::vector<vsize> Line_division;

  /*
    Determine the page breaking, and break scores into lines
    appropriately.

    This is the only external entry point. Other public methods are
    for page breaking util classes to query the problem.
   */
  virtual SCM solve () = 0;

  Page_breaking (Paper_book *pb, Break_predicate, Prob_break_predicate);
  virtual ~Page_breaking ();

  bool ragged () const;
  bool ragged_last () const;
  bool is_last () const;
  bool ends_score () const;
  int systems_per_page () const;
  int max_systems_per_page () const;
  int min_systems_per_page () const;
  Real page_height (int page_number, bool last) const;
  Real paper_height () const;
  vsize system_count () const;
  Real footnote_separator_stencil_height () const;
  Real footnote_padding () const;
  Real in_note_padding () const;
  Real footnote_number_raise () const;
  Real footnote_footer_padding () const;
  Real line_count_penalty (int line_count) const;
  int line_count_status (int line_count) const;
  bool too_many_lines (int line_count) const;
  bool too_few_lines (int line_count) const;
  Real min_whitespace_at_top_of_page (Line_details const &) const;
  Real min_whitespace_at_bottom_of_page (Line_details const &) const;
  int orphan_penalty () const;

protected:
  // This naming sounds weird, shouldn't this be paper_book_?
  Paper_book *book_;

  vsize next_system (Break_position const &break_pos) const;

  SCM make_pages (const std::vector<vsize> &lines_per_page, SCM lines);

  vsize min_system_count (vsize start, vsize end);
  vsize max_system_count (vsize start, vsize end);

  void break_into_pieces (vsize start, vsize end, Line_division const &div);
  SCM systems ();
  SCM footnotes ();

  void set_current_breakpoints (vsize start, vsize end, vsize system_count,
                                Line_division lower_bound = Line_division (),
                                Line_division upper_bound = Line_division ());
  void set_to_ideal_line_configuration (vsize start, vsize end);

  vsize current_configuration_count () const;
  Line_division current_configuration (vsize configuration_index) const;
  Page_spacing_result space_systems_on_n_pages (vsize configuration_index,
                                                vsize n, int first_page_num);
  Page_spacing_result
  space_systems_on_n_or_one_more_pages (vsize configuration_index, vsize n,
                                        int first_page_num,
                                        Real penalty_for_fewer_pages);
  Page_spacing_result space_systems_on_best_pages (vsize configuration_index,
                                                   int first_page_num);
  Page_spacing_result
  space_systems_with_fixed_number_per_page (vsize configuration_index,
                                            int first_page_num);
  Page_spacing_result pack_systems_on_least_pages (vsize configuration_index,
                                                   int first_page_num);
  vsize min_page_count (vsize configuration_index, int first_page_num);
  bool all_lines_stretched (vsize configuration_index);
  Real blank_page_penalty () const;

  SCM breakpoint_property (vsize breakpoint, char const *str);

  vsize last_break_position () const;

  std::vector<System_spec> system_specs_;

private:
  std::vector<Break_position> breaks_;
  std::vector<Break_position> chunks_;
  std::vector<Constrained_breaking> line_breaking_;
  bool ragged_;
  bool ragged_last_;
  int systems_per_page_;
  int max_systems_per_page_;
  int min_systems_per_page_;
  vsize system_count_;
  Real footnote_separator_stencil_height_;
  Real footnote_padding_;
  Real in_note_padding_;
  Real footnote_number_raise_;
  Real footnote_footer_padding_;
  int orphan_penalty_;

  std::vector<Line_division> current_configurations_;
  std::vector<Break_position> current_chunks_;
  vsize current_start_breakpoint_;
  vsize current_end_breakpoint_;

  void cache_line_details (vsize configuration_index);
  void calc_line_heights ();
  void clear_line_details_cache ();
  vsize cached_configuration_index_;
  std::vector<Line_details> cached_line_details_;
  std::vector<Line_details> uncompressed_line_details_;

  Real paper_height_;
  mutable std::vector<Real> page_height_cache_;
  mutable std::vector<Real> last_page_height_cache_;

  std::vector<Break_position> chunk_list (vsize start, vsize end) const;
  Line_division system_count_bounds (std::vector<Break_position> const &chunks,
                                     bool min);
  void line_breaker_args (vsize i, Break_position const &start,
                          Break_position const &end, vsize *line_breaker_start,
                          vsize *line_breaker_end);

  void line_divisions_rec (vsize system_count, Line_division const &min,
                           Line_division const &max, Line_division *cur);

  std::vector<Line_details> line_details (vsize start, vsize end,
                                          Line_division const &div);
  Page_spacing_result
  space_systems_on_1_page (std::vector<Line_details> const &lines,
                           Real page_height, bool ragged);
  Page_spacing_result space_systems_on_2_pages (vsize configuration_index,
                                                int first_page_num);
  Page_spacing_result finalize_spacing_result (vsize configuration_index,
                                               Page_spacing_result);
  void create_system_list ();
  void find_chunks_and_breaks (Break_predicate, Prob_break_predicate);
  SCM draw_page (SCM systems, int page_num, bool last, Real &last_page_force);
};
#endif /* PAGE_BREAKING_HH */
