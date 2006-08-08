/*
  page-breaking.hh -- declare a superclass and utility
  functions for several different page-breaking algorithms

  source file of the GNU LilyPond music typesetter

  (c) 2006 Joe Neeman <joeneeman@gmail.com>
*/

#ifndef PAGE_BREAKING_HH
#define PAGE_BREAKING_HH

#include "constrained-breaking.hh"
#include "lily-guile.hh"

/* Either a paper-score, markup or header.
 */
struct System_spec
{
  System_spec (Paper_score*, int);
  System_spec (Prob*);
  System_spec ();

  Paper_score *pscore_;
  Prob *prob_;

  vsize score_break_count_; /* for scores, this is the number of start-points our line-
                               breaker has */
};

struct Break_position
{
  vsize sys_; /* the index in our all_ list */
  vsize score_break_; /* if sys_ is a score, then we start at the score_brk_'th
                         possible page-break in the score */

  Break_position (vsize s=VPOS, vsize brk=VPOS)
  {
    sys_ = s;
    score_break_ = brk;
  }
};

class Page_breaking
{
public:
  virtual SCM solve () = 0;

  Page_breaking (Paper_book *pb, bool allow_intra_score_breaks);
  virtual ~Page_breaking ();

protected:
  std::vector<Break_position> breaks_;
  std::vector<System_spec> all_;
  std::vector<Constrained_breaking> line_breaking_;

  Paper_book *book_;

  Real page_height (int page_number, bool last);
  vsize next_system (vsize starting_break_index) const;

  void create_system_list (bool allow_intra_score_breaks);
  std::vector<vsize> find_page_break_indices (Paper_score *pscore);

  SCM make_pages (vector<vsize> lines_per_page, SCM lines);

  void calc_system_count_bounds (vsize start, vsize end,
                                 vector<vsize> *min,
                                 vector<vsize> *max);

  void divide_systems (vsize system_count, vector<vsize> const &min,
                                           vector<vsize> const &max,
                                           vector<vector<vsize> > *result,
                                           vector<vsize> *cur);

  void line_breaker_args (vsize i, vsize *break_st, vsize *break_end);
  vsize get_min_systems (vsize sys, vsize break_start, vsize break_end);
  vsize get_max_systems (vsize sys, vsize break_start, vsize break_end);
  vector<Column_x_positions> get_line_breaks (vsize sys, vsize sys_count, vsize st, vsize end);
  vector<Line_details> get_line_details (vsize start_break, vsize end_break, vector<vsize> const &div);
};
#endif /* PAGE_BREAKING_HH */
