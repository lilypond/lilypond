/*
  script-column.hh -- declare Script_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCRIPT_COLUMN_HH
#define SCRIPT_COLUMN_HH

#include "axis-group-item.hh"

/** a struct for treating a group of noteheads (noteheads, stem
  (chord) and scripts) as a single entity.  */
class Script_column : public Axis_group_item {

protected:
  virtual void do_print() const;
  virtual void do_substitute_element_pointer (Score_element*, Score_element*);
  virtual void do_pre_processing() ;
public:
  Link_array<Script> script_l_arr_;
  Link_array<Item> support_l_arr_;
    
   
  virtual void add_script (Script *);
  void add_support (Item*);
  Script_column ();
};

#endif // SCRIPT_COLUMN_HH
