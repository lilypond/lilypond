/*
  staff-spacing.hh -- declare Staff_spacing

  source file of the GNU LilyPond music typesetter

  (c) 2001--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef STAFF_SPACING_HH
#define STAFF_SPACING_HH

#include "lily-proto.hh"
#include "grob-interface.hh"
#include "spring.hh"

class Staff_spacing
{
  static Real optical_correction (Grob *, Grob *, Interval);
  static Real next_notes_correction (Grob *, Grob *);

public:
  DECLARE_GROB_INTERFACE();
  static Spring get_spacing (Grob *, Grob *right_col);
  static Interval bar_y_positions (Grob *);
};

#endif /* STAFF_SPACING_HH */
