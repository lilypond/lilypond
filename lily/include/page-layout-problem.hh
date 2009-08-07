/*
  page-layout-problem.hh -- space systems nicely on a page. If systems can
  be stretched, do that too.

  source file of the GNU LilyPond music typesetter

  (c) 2009 Joe Neeman <joeneeman@gmail.com>
*/

#ifndef PAGE_LAYOUT_HH
#define PAGE_LAYOUT_HH

#include "simple-spacer.hh"
#include "skyline.hh"

class Page_layout_problem
{
public:
  Page_layout_problem (Paper_book*, SCM page, SCM systems);

  SCM solution (bool ragged);
  void set_header_height (Real);
  void set_footer_height (Real);
  static bool read_spacing_spec (SCM spec, Real* dest, SCM sym);
  static bool is_spaceable (Grob *g);
  static SCM get_details (Grob *g);
  static SCM get_spacing_spec (Grob *before, Grob *after);

protected:
  void append_system (System*, Spring const&, Real padding);
  void append_prob (Prob*, Spring const&, Real padding);

  void solve_rod_spring_problem (bool ragged);
  SCM find_system_offsets ();
  void distribute_loose_lines (vector<Grob*> const&, vector<Real> const&, Real, Real);
  void add_loose_lines_as_spaceable_lines (vector<Grob*> const&,
					   vector<Real> const&,
					   vsize start, vsize end);

  static Grob* find_vertical_alignment (System*);
  static void build_system_skyline (vector<Grob*> const&, vector<Real> const&, Skyline* up, Skyline* down);
  static vector<Grob*> filter_dead_elements (vector<Grob*> const&);

  // This is a union (in spirit).
  // Either staves must be empty or prob must be null.
  typedef struct Element {
    Prob *prob;
    vector<Grob*> staves;
    vector<Real> min_offsets;

    Element (vector<Grob*> const& a, vector<Real> const& o)
    {
      staves = a;
      min_offsets = o;
      prob = 0;
    }

    Element (Prob *p)
    {
      prob = p;
    }
  } Element;

  static Interval first_staff_extent (Element const&);
  static Interval last_staff_extent (Element const&);
  static Interval prob_extent (Prob*);
  static SCM get_details (Element const&);
  static SCM details_get_property (SCM details, const char*);
  static void alter_spring_from_spacing_spec (SCM spec, Spring* spring);
  static void mark_as_spaceable (Grob*);

  vector<Spring> springs_;
  vector<Element> elements_;
  vector<Real> solution_;
  Skyline bottom_skyline_;
  Real page_height_;
  Real header_height_;
  Real footer_height_;
};

#endif /* PAGE_LAYOUT_HH */
