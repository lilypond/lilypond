/*
  note-spacing.hh -- declare Note_spacing

  source file of the GNU LilyPond music typesetter

  (c) 2001--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef NOTE_SPACING_HH
#define NOTE_SPACING_HH

#include "lily-proto.hh"

class Note_spacing
{
public:
  static bool has_interface (Grob *);

  static void get_spacing (Grob *me, Item *, Real, Real, Real *, Real *);
  static void stem_dir_correction (Grob *me, Item *next_col, Real incr,
				   Real *, Real *);
  static Item *right_column (Grob *);
  static Item *left_column (Grob *);
};

#endif /* NOTE_SPACING_HH */
