/*
  script-column.hh -- declare Script_column

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCRIPT_COLUMN_HH
#define SCRIPT_COLUMN_HH

#include "item.hh"

/** a struct for treating a group of noteheads (noteheads, stem
  (chord) and scripts ) as a single entity.  */
class Script_column : public Item {
protected:
    virtual Interval do_height()const;
    virtual Interval do_width()const;
    virtual void do_print() const ;
    virtual void do_pre_processing();
public:
    Array<Script *> script_l_arr_;
    Array<Item *> support_l_arr_;
    
    NAME_MEMBERS(Script_column);
    virtual void translate(Offset);
    void add(Script *);
    void add_support(Item*);
};

#endif // SCRIPT_COLUMN_HH
