/*
  note-column.hh -- declare Note_column

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef NOTE_COLUMN_HH
#define NOTE_COLUMN_HH

#include "item.hh"
#include "head-column.hh"

/** a struct for treating a group of noteheads (noteheads, stem
  (chord) and scripts) as a single entity.  */
class Note_column : public Head_column {
protected:
    virtual void do_pre_processing();

public:
    bool h_shift_b_;
    
    Interval_t<int> head_positions_interval()const;
        
    DECLARE_MY_RUNTIME_TYPEINFO;
    Note_column();
    virtual void set (Stem *);
    void sort();
};

#endif // NOTE_COLUMN_HH
