/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2009--2022 Joe Neeman <joeneeman@gmail.com>

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

#ifndef PAGE_LAYOUT_HH
#define PAGE_LAYOUT_HH

#include "simple-spacer.hh"
#include "skyline.hh"
#include "stencil.hh"

#include <vector>

class Page_layout_problem
{
public:
  Page_layout_problem (Paper_book *, SCM page, SCM systems);

  SCM solution (bool ragged);
  SCM fixed_force_solution (Real force);
  void set_header_height (Real);
  void set_footer_height (Real);
  Real force () const;
  static bool read_spacing_spec (SCM spec, Real *dest, SCM sym);
  static bool is_spaceable (Grob *g);
  static SCM get_details (Spanner *);
  static std::vector<Grob *> get_footnote_grobs (SCM lines);
  static vsize get_footnote_count (SCM lines);
  static SCM get_footnotes_from_lines (SCM lines);
  static void add_footnotes_to_lines (SCM lines, vsize counter, Paper_book *pb);
  static Stencil get_footnote_separator_stencil (Output_def *paper);
  static SCM get_spacing_spec (Grob *before, Grob *after, bool pure,
                               vsize start, vsize end);
  static Real get_fixed_spacing (Grob *before, Grob *after, int spaceable_index,
                                 bool pure, vsize start, vsize end);
  static Stencil add_footnotes_to_footer (SCM footnotes, Stencil foot,
                                          Paper_book *pb);

protected:
  void append_system (System *, Spring const &, Real indent, Real padding);
  void append_prob (Prob *, Spring const &, Real padding);

  void solve_rod_spring_problem (bool ragged, Real fixed_force);
  SCM find_system_offsets ();
  void distribute_loose_lines (std::vector<Grob *> const &,
                               std::vector<Real> const &, Real, Real);

  static void build_system_skyline (std::vector<Grob *> const &,
                                    std::vector<Real> const &, Skyline *up,
                                    Skyline *down);
  static std::vector<Grob *> filter_dead_elements (std::vector<Grob *> const &);

  // This is a union (in spirit).
  // Either staves must be empty or prob must be null.
  typedef struct Element
  {
    Prob *prob;
    std::vector<Grob *> staves;
    std::vector<Real> min_offsets;
    // Store the appropriate '*-*-spacing 'padding, and skyline-distance,
    //  considering indentation, from the previous system.
    Real min_distance;
    Real padding;

    Element (std::vector<Grob *> const &a, std::vector<Real> const &o, Real m,
             Real p)
    {
      staves = a;
      min_offsets = o;
      min_distance = m;
      padding = p;
      prob = 0;
    }

    Element (Prob *p, Real pad)
    {
      prob = p;
      padding = pad;
      // min_distance is initialized to avoid -Wmaybe-uninitialized warnings
      // which are possibly false positives.
      min_distance = 0;
    }
  } Element;

  static Interval first_staff_extent (Element const &);
  static Interval last_staff_extent (Element const &);
  static Interval prob_extent (Prob *);
  static SCM get_details (Element const &);
  static SCM details_get_property (SCM details, const char *);
  static void alter_spring_from_spacing_spec (SCM spec, Spring *spring);
  static void mark_as_spaceable (Grob *);

  std::vector<Spring> springs_;
  std::vector<Element> elements_;
  std::vector<Real> solution_;
  Real force_;
  Skyline bottom_skyline_;
  Real bottom_loose_baseline_;
  Real page_height_;
  Real header_height_;
  Real footer_height_;
  Real header_padding_;
  Real footer_padding_;
  Real in_note_padding_;
  Direction in_note_direction_;
};

#endif /* PAGE_LAYOUT_HH */
