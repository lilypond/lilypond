/*
  note-column.hh -- declare Note_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef NOTE_COLUMN_HH
#define NOTE_COLUMN_HH

#include "item.hh"

/** a struct for treating a group of noteheads (noteheads, stem
 (chord) and scripts) as a single entity.

  UGR. Junkme.  refpoint should be the notehead, dir should come from stem.

*/
class Note_column
{
public:
  static int shift_compare (Grob *const &, Grob*const&);
  
  /** The relative position of the "voice" containing this
    chord. Normally this would be the same as the stem direction,

    JUNKME.
    */

  static Grob * accidentals (Grob*me);
  static Direction dir (Grob*me);
  static Slice head_positions_interval (Grob* me);
  static Direction static_dir (Grob*);
  static void translate_rests (Grob*me,int dy);
  static Grob * first_head (Grob*me);
  static void set_stem (Grob*me,Grob*);
  static void set_dotcol (Grob*me,Grob*);
  static void add_head (Grob*me,Grob*);
  static bool rest_b (Grob*me);
  static bool has_interface (Grob*);
  static void set_interface (Grob*);
  static Item *stem_l (Grob*);
};

#endif // NOTE_COLUMN_HH
