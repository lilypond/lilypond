/*
  page-spacing.hh -- routines for spacing systems
  vertically across pages

  source file of the GNU LilyPond music typesetter

  (c) 2006--2009 Joe Neeman <joeneeman@gmail.com>
*/

#ifndef PAGE_SPACING_HH
#define PAGE_SPACING_HH

#include "constrained-breaking.hh"
#include "page-spacing-result.hh"


/* for page_count > 2, we use a dynamic algorithm similar to
   constrained-breaking -- we have a class that stores the intermediate
   calculations so they can be reused for querying different page counts.
*/
class Page_spacer
{
public:
  Page_spacer (vector<Line_details> const &lines, vsize first_page_num, Page_breaking const*);
  Page_spacing_result solve (vsize page_count);

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

  Page_breaking const *breaker_;
  vsize first_page_num_;
  vector<Line_details> lines_;
  Matrix<Page_spacing_node> state_;
  vsize max_page_count_;

  bool ragged_;
  bool ragged_last_;

  void resize (vsize page_count);
  bool calc_subproblem (vsize page, vsize lines);
};

struct Page_spacing
{
  Real force_;
  Real page_height_;
  Real rod_height_;
  Real spring_len_;
  Real inverse_spring_k_;
  Real page_top_space_;

  Line_details last_line_;
  Line_details first_line_;

  Page_spacing (Real page_height, Real page_top_space)
  {
    page_height_ = page_height;
    page_top_space_ = page_top_space;
    clear ();
  }

  void calc_force ();
  void resize (Real new_height);
  void append_system (const Line_details &line);
  void prepend_system (const Line_details &line);
  void clear ();
};

#endif /* PAGE_SPACING_HH */
