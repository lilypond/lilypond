/*
  note-column.hh -- declare Note_column

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef NOTE_COLUMN_HH
#define NOTE_COLUMN_HH
#include "item.hh"

/** 
  a struct for treating a group of noteheads (noteheads, stem, scripts )
  as a single entity.  
  */
class Note_column : public Item {
    Stem * stem_l_;
    Array<Notehead*> head_l_arr_;
    Array<Script *> script_l_arr_;
    
protected:
    virtual void translate(Offset);
    virtual  Interval do_height()const;
    virtual void do_print() const ;
    virtual void do_pre_processing();
    virtual Interval do_width()const;
public:
    
    NAME_MEMBERS(Note_column);
    Note_column();
    void add(Notehead *);
    void add(Stem *);
    void add(Script *);
};
#endif // NOTE_COLUMN_HH
