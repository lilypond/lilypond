/*
  note-column.hh -- declare Note_column

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef NOTE_COLUMN_HH
#define NOTE_COLUMN_HH
#include "item.hh"
#include "interval.hh"

/** a struct for treating a group of noteheads (noteheads, stem
  (chord) and scripts ) as a single entity.  */
class Note_column : public Item {
    virtual  Interval do_height()const;
    virtual void do_print() const ;
    virtual void do_pre_processing();
    virtual Interval do_width()const;
public:
    bool h_shift_b_;
    Stem * stem_l_;
    Array<Script *> script_l_arr_;
    bool rest_b_;
    Array<Notehead*> head_l_arr_;
    
    /** The relative position of the "voice" containing this
      chord. Normally this would be the same as the stem direction,
      but rests do not have stems.  */
    int dir_i_;
   
    Interval_t<int> head_positions_interval()const;
    
    NAME_MEMBERS(Note_column);
    Note_column();
    void add(Notehead *);
    void add(Stem *);
    virtual void translate(Offset);
    void sort();
    void add(Script *);
};
#endif // NOTE_COLUMN_HH
