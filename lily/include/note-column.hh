/*
  note-column.hh -- declare Note_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  static int shift_compare (Score_element *const &, Score_element*const&);
  
  /** The relative position of the "voice" containing this
    chord. Normally this would be the same as the stem direction,

    JUNKME.
    */

  static Direction dir (Score_element*me);
  static Slice head_positions_interval(Score_element* me);
  static Direction static_dir (Score_element*);
  static void translate_rests(Score_element*me,int dy);
  static Score_element * first_head (Score_element*me);
  static Interval rest_dim (Score_element*me);
  static void set_stem (Score_element*me,Score_element*);
  static void set_dotcol (Score_element*me,Score_element*);
  static void add_head (Score_element*me,Score_element*);
  static bool rest_b (Score_element*me);
  static bool has_interface (Score_element*);
  static void set_interface (Score_element*);
  static Item *stem_l(Score_element*);
};

#endif // NOTE_COLUMN_HH
