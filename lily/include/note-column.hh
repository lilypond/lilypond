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
class Note_column : public Item
{
public:
  static int shift_compare (Note_column *const &, Note_column*const&);
  
  /** The relative position of the "voice" containing this
    chord. Normally this would be the same as the stem direction,

    JUNKME.
    */
  Direction dir () const;


  static Slice head_positions_interval(Score_element* me) ;
  static Direction static_dir (Score_element*);
  void translate_rests(int dy);
  Note_head * first_head ()const;
  Interval rest_dim ()const ;
  Note_column (SCM);
  void set_stem (Score_element*);
  void set_dotcol (Score_element*);
  void add_head (Score_element*);
  bool rest_b () const;

  Stem *stem_l()const;
};

#endif // NOTE_COLUMN_HH
