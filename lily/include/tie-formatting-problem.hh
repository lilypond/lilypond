/*
  tie-formatting-problem.hh -- declare

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef TIE_FORMATTING_PROBLEM_HH
#define TIE_FORMATTING_PROBLEM_HH

#include "drul-array.hh"
#include "parray.hh"
#include "skyline.hh"
#include "lily-proto.hh"

class Tie_formatting_problem
{
  Drul_array< Array<Skyline_entry> > chord_outlines_;
  Grob *x_refpoint_;
public:
  Tie_formatting_problem ();

  void from_ties (Link_array<Grob> const &ties);
  void from_tie (Grob *tie);
  void from_lv_ties (Link_array<Grob> const &);
  void set_chord_outline (Link_array<Item>, Direction);

  Interval get_attachment (Real) const;
  Grob *common_x_refpoint () const;
};

#endif /* TIE_FORMATTING_PROBLEM_HH */
