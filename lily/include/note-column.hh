/*
  note-column.hh -- declare Note_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef NOTE_COLUMN_HH
#define NOTE_COLUMN_HH

#include "item.hh"
#include "axis-group-item.hh"

/** a struct for treating a group of noteheads (noteheads, stem
  (chord) and scripts) as a single entity.  */
class Note_column : public Axis_group_item {
protected:
  virtual void do_post_processing () ;
public:

  static int shift_compare (Note_column *const &, Note_column*const&);
  
  /** The relative position of the "voice" containing this
    chord. Normally this would be the same as the stem direction,

    JUNKME.
    */
  Direction dir () const;


  Interval_t<int> head_positions_interval() const;
  //  Interval width () const;

  void translate_rests(int dy);
  Note_head * first_head ()const;
  Interval rest_dim ()const ;
  Note_column ();
  void set_stem (Stem*);
  void set_dotcol (Dot_column*);
  void add_head (Rhythmic_head*);
  bool rest_b () const;

  Stem *stem_l()const;
};

#endif // NOTE_COLUMN_HH
