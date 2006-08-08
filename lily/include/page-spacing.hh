/*
  page-spacing.hh -- routines for spacing systems
  vertically across pages

  source file of the GNU LilyPond music typesetter

  (c) 2006 Joe Neeman <joeneeman@gmail.com>
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
    demerits_ = 0;
  }
};

Spacing_result
space_systems_on_min_pages (vector<Line_details> const&,
			    Real page_height,
			    Real odd_pages_penalty);
Spacing_result
space_systems_on_best_pages (vector<Line_details> const&,
			     Real page_height,
			     Real odd_pages_penalty);

#endif /* PAGE_SPACING_HH */
