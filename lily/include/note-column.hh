/*
  note-column.hh -- declare Note_column

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef NOTE_COLUMN_HH
#define NOTE_COLUMN_HH
#include "item.hh"
#include "script-column.hh"

/** a struct for treating a group of noteheads (noteheads, stem
  (chord) and scripts ) as a single entity.  */
class Note_column : public Script_column {
    void do_pre_processing();
public:
        /// link to the stem. For setting default direction
    Stem * stem_l_;

    bool h_shift_b_;
    
    Array<Notehead*> head_l_arr_;
    Interval_t<int> head_positions_interval()const;

    /** The relative position of the "voice" containing this
      chord. Normally this would be the same as the stem direction,
      but rests do not have stems.  */
    int dir_i_;
   
        
    NAME_MEMBERS(Note_column);
    Note_column();
    void add(Notehead *);
    void add(Stem *);
    void sort();
    void add(Script* s) { Script_column::add(s);}
};

#endif // NOTE_COLUMN_HH
