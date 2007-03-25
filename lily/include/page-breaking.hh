/*
  page-breaking.hh -- declare a superclass and utility
  functions for several different page-breaking algorithms

  source file of the GNU LilyPond music typesetter

  (c) 2006--2007 Joe Neeman <joeneeman@gmail.com>
*/

#ifndef PAGE_BREAKING_HH
#define PAGE_BREAKING_HH

#include "constrained-breaking.hh"
#include "page-spacing.hh"

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

  System_spec ()
  {
    pscore_ = NULL;
    prob_ = NULL;
  }

  Paper_score *pscore_;
  Prob *prob_;
};

struct Break_position
{
  vsize sys_; /* our index in the all_ list */
  vsize score_break_; /* if sys_ is a score, then we start at the score_brk_'th
                         possible page-break in the score */
  Grob *col_;  /* if sys_ is a score, this points to the broken column */
  bool score_ender_;

  Break_position (vsize s=VPOS, vsize brk=VPOS, Grob *g=NULL, bool end=false)
  {
    sys_ = s;
    score_break_ = brk;
    col_ = g;
    score_ender_ = end;
  }

  bool operator< (const Break_position &other)
  {
    return (sys_ == VPOS && other.sys_ != VPOS)
      || (sys_ < other.sys_)
      || (sys_ == other.sys_ && score_break_ < other.score_break_);
  }

  bool operator<= (const Break_position &other)
  {
    return (sys_ == VPOS)
      || (sys_ < other.sys_ && other.sys_ != VPOS)
      || (sys_ == other.sys_ && score_break_ <= other.score_break_);
  }
};

class Page_breaking
{
public:
  typedef bool (*Break_predicate) (Grob *);
  typedef vector<vsize> Line_division;
  virtual SCM solve () = 0;

  Page_breaking (Paper_book *pb, Break_predicate);
  virtual ~Page_breaking ();

  bool ragged () const;
  bool ragged_last () const;
  bool last () const;
  Real page_height (int page_number, bool last) const;

protected:
  Paper_book *book_;

  vsize next_system (Break_position const &break_pos) const;

  SCM make_pages (vector<vsize> lines_per_page, SCM lines);

  vsize min_system_count (vsize start, vsize end);
  vsize max_system_count (vsize start, vsize end);


  void break_into_pieces (vsize start, vsize end, Line_division const &div);
  SCM systems ();

  void set_current_breakpoints (vsize start,
				vsize end,
				vsize system_count,
				Line_division lower_bound = Line_division (),
				Line_division upper_bound = Line_division ());
  vsize current_configuration_count () const;
  Line_division current_configuration (vsize configuration_index) const;
  Spacing_result space_systems_on_n_pages (vsize configuration_index, vsize n, vsize first_page_num);
  Spacing_result space_systems_on_n_or_one_more_pages (vsize configuration_index, vsize n, vsize first_page_num);
  Spacing_result space_systems_on_best_pages (vsize configuration_index, vsize first_page_num);
  vsize min_page_count (vsize configuration_index, vsize first_page_num);
  bool all_lines_stretched (vsize configuration_index);
  Real blank_page_penalty () const;

  SCM breakpoint_property (vsize breakpoint, char const *str);
  vector<Break_position> breaks_;

private:
  vector<Break_position> chunks_;
  vector<System_spec> all_;
  vector<Constrained_breaking> line_breaking_;
  bool ragged_;
  bool ragged_last_;

  vector<Line_division> current_configurations_;
  vector<Break_position> current_chunks_;
  vsize current_start_breakpoint_;
  vsize current_end_breakpoint_;

  void cache_line_details (vsize configuration_index);
  void clear_line_details_cache ();
  vsize cached_configuration_index_;
  vector<Line_details> cached_line_details_;

  vector<Break_position> chunk_list (vsize start, vsize end);
  Line_division system_count_bounds (vector<Break_position> const &chunks, bool min);
  void line_breaker_args (vsize i,
			  Break_position const &start,
			  Break_position const &end,
			  vsize *line_breaker_start,
			  vsize *line_breaker_end);

  void line_divisions_rec (vsize system_count,
			   Line_division const &min,
			   Line_division const &max,
			   Line_division *cur);

  vector<Line_details> line_details (vsize start, vsize end, Line_division const &div);
  Spacing_result space_systems_on_1_page (vector<Line_details> const &lines, Real page_height, bool ragged);
  Spacing_result space_systems_on_2_pages (vsize configuration_index, vsize first_page_num);
  Spacing_result finalize_spacing_result (vsize configuration_index, Spacing_result);
  void create_system_list ();
  void find_chunks_and_breaks (Break_predicate);
};
#endif /* PAGE_BREAKING_HH */
