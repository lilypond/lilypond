/*
  page-turn-page-breaking.hh -- break lines and pages optimally
  for a whole Paper_book such that page turns can only occur
  at specific places.

  source file of the GNU LilyPond music typesetter

  (c) 2006--2008 Joe Neeman <joeneeman@gmail.com>
*/

#ifndef PAGE_TURN_PAGE_BREAKING_HH
#define PAGE_TURN_PAGE_BREAKING_HH

#include "page-breaking.hh"

/*
  A dynamic programming solution to breaking pages
 */
class Page_turn_page_breaking: public Page_breaking
{
public:
  virtual SCM solve ();

  Page_turn_page_breaking (Paper_book *pb);
  virtual ~Page_turn_page_breaking ();

protected:
  struct Break_node
  {
    vsize prev_;
    int first_page_number_;
    vsize page_count_;

    /* true if every score here is too widely spaced */
    bool too_many_lines_;

    Real demerits_;
    vsize break_pos_; /* index into breaks_ */

    Line_division div_;
    vector<vsize> system_count_; /* systems per page */

    Break_node ()
    {
      prev_ = break_pos_ = VPOS;
      demerits_ = infinity_f;
      first_page_number_ = 0;
      page_count_ = 0;
      too_many_lines_ = false;
    }
  };

  vector<Break_node> state_;

  vsize total_page_count (Break_node const &b);
  Break_node put_systems_on_pages (vsize start,
				   vsize end,
				   vsize configuration,
				   vsize page_number);

  SCM make_lines (vector<Break_node> *breaks);
  SCM make_pages (vector<Break_node> const &breaks, SCM systems);

  void calc_subproblem (vsize i);
  void print_break_node (Break_node const &b);
};
#endif /* PAGE_TURN_PAGE_BREAKING_HH */
