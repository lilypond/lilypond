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
*/
class Note_column : public Item
{
public:
  SCM member_after_line_breaking ();
  static SCM after_line_breaking (SCM);
  static int shift_compare (Note_column *const &, Note_column*const&);
  
  /** The relative position of the "voice" containing this
    chord. Normally this would be the same as the stem direction,

    JUNKME.
    */
  Direction dir () const;


  Interval_t<int> head_positions_interval() const;

  void translate_rests(int dy);
  Note_head * first_head ()const;
  Interval rest_dim ()const ;
  Note_column (SCM);
  void set_stem (Stem*);
  void set_dotcol (Dot_column*);
  void add_head (Rhythmic_head*);
  bool rest_b () const;

  Stem *stem_l()const;
};

#endif // NOTE_COLUMN_HH
