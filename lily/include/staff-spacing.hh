/*
  staff-spacing.hh -- declare Staff_spacing

  source file of the GNU LilyPond music typesetter

  (c) 2001--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef STAFF_SPACING_HH
#define STAFF_SPACING_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

class Staff_spacing
{
public:
  static void next_notes_correction (Grob *, Grob *, Real, Real, Real *, Real *);
  static void next_note_correction (Grob *, Grob *, Interval, Real, Real, Real*, Real *, int *);
  DECLARE_GROB_INTERFACE();
  static void get_spacing_params (Grob *, Real *, Real *);

  static Interval bar_y_positions (Grob *);
};

#endif /* STAFF_SPACING_HH */
