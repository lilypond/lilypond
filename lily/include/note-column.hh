/*
  note-column.hh -- declare Note_column

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef NOTE_COLUMN_HH
#define NOTE_COLUMN_HH

#include "grob-interface.hh"
#include "lily-proto.hh"


/** a struct for treating a group of noteheads (noteheads, stem
    (chord) and scripts) as a single entity.

    UGR. Junkme.  refpoint should be the notehead, dir should come from stem.
*/
class Note_column
{
public:
  static bool shift_less (Grob *const &, Grob *const &);
  static Direction dir (Grob *me);
  static Grob *accidentals (Grob *me);
  static Grob *arpeggio (Grob *me);
  static Slice head_positions_interval (Grob *me);
  static void translate_rests (Grob *me, int dy);
  static Grob *first_head (Grob *me);
  static Grob *get_rest (Grob *me);
  static void set_stem (Grob *me, Grob *);
  static void add_head (Grob *me, Grob *);
  static bool has_rests (Grob *me);
  static Grob *dot_column (Grob *me);
  static Interval cross_staff_extent (Grob *me, Grob *refp);
  DECLARE_GROB_INTERFACE();

  static Item *get_stem (Grob *);
};

#endif // NOTE_COLUMN_HH
