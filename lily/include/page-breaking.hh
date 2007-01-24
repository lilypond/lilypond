/*
  page-breaking.hh -- declare a superclass and utility
  functions for several different page-breaking algorithms

  source file of the GNU LilyPond music typesetter

  (c) 2006--2007 Joe Neeman <joeneeman@gmail.com>
*/

#ifndef PAGE_BREAKING_HH
#define PAGE_BREAKING_HH

#include "constrained-breaking.hh"

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

protected:
  Paper_book *book_;

  Real page_height (int page_number, bool last);
  vsize next_system (Break_position const &break_pos) const;

  SCM make_pages (vector<vsize> lines_per_page, SCM lines);

  vsize min_system_count (vsize start, vsize end);
  vsize max_system_count (vsize start, vsize end);
  vector<Line_details> line_details (vsize start, vsize end, Line_division const &div);

  void break_into_pieces (vsize start, vsize end, Line_division const &div);
  SCM systems ();


  vector<Line_division> line_divisions (vsize start,
					vsize end,
					vsize system_count,
					Line_division lower_bound = Line_division (),
					Line_division upper_bound = Line_division ());

  SCM breakpoint_property (vsize breakpoint, char const *str);
  vector<Break_position> breaks_;

private:
  vector<Break_position> chunks_;
  vector<System_spec> all_;
  vector<Constrained_breaking> line_breaking_;

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
			   vector<Line_division> *result,
			   Line_division *cur);

  void create_system_list ();
  void find_chunks_and_breaks (Break_predicate);
};
#endif /* PAGE_BREAKING_HH */
