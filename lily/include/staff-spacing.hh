/*
  staff-spacing.hh -- declare Staff_spacing

  source file of the GNU LilyPond music typesetter

  (c) 2001--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef STAFF_SPACING_HH
#define STAFF_SPACING_HH

#include "lily-proto.hh"

class Staff_spacing
{
public:
  static void next_notes_correction (Grob *, Grob *, Real *, Real *);
  static void next_note_correction (Grob *, Grob *, Interval, Real*, Real *);
  static bool has_interface (Grob *);
  static void get_spacing_params (Grob *, Real *, Real *);

  static Interval bar_y_positions (Grob *);
};

#endif /* STAFF_SPACING_HH */
