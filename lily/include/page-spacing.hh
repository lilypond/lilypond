/*
  page-spacing.hh -- routines for spacing systems
  vertically across pages

  source file of the GNU LilyPond music typesetter

  (c) 2006--2007 Joe Neeman <joeneeman@gmail.com>
*/

#ifndef PAGE_SPACING_HH
#define PAGE_SPACING_HH

#include "constrained-breaking.hh"

struct Spacing_result {
  vector<vsize> systems_per_page_;
  vector<Real> force_;
  Real penalty_;
  Real demerits_;

  Spacing_result ()
  {
    penalty_ = 0;
    demerits_ = infinity_f;
  }
};

/* for page_count > 2, we use a dynamic algorithm similar to
   constrained-breaking -- we have a class that stores the intermediate
   calculations so they can be reused for querying different page counts.
*/

class Page_spacer
{
public:
  Page_spacer (vector<Line_details> const &lines, Real page_height, bool ragged, bool ragged_last);
  Spacing_result solve (vsize page_count);

private:
  struct Page_spacing_node
  {
    Page_spacing_node ()
    {
      demerits_ = infinity_f;
      force_ = infinity_f;
      penalty_ = infinity_f;
      prev_ = VPOS;
    }

    Real demerits_;
    Real force_;
    Real penalty_;
    vsize prev_;
  };

  Real page_height_;
  vector<Line_details> lines_;
  Matrix<Page_spacing_node> state_;
  vsize max_page_count_;

  bool ragged_;
  bool ragged_last_;

  void resize (vsize page_count);
  bool calc_subproblem (vsize page, vsize lines);
};

vsize
min_page_count (vector<Line_details> const &lines,
		Real page_height, bool ragged, bool ragged_last);

Spacing_result
space_systems_on_n_pages (vector<Line_details> const&,
			  vsize n,
			  Real page_height,
			  bool ragged,
			  bool ragged_last);

Spacing_result
space_systems_on_n_or_one_more_pages (vector<Line_details> const&,
				      vsize n,
				      Real page_height,
				      Real odd_pages_penalty,
				      bool ragged,
				      bool ragged_last);
Spacing_result
space_systems_on_best_pages (vector<Line_details> const&,
			     Real page_height,
			     Real odd_pages_penalty,
			     bool ragged,
			     bool ragged_last);

#endif /* PAGE_SPACING_HH */
