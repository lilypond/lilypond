/*
  rest-column.hh -- declare Rest_column

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef REST_COLUMN_HH
#define REST_COLUMN_HH

#include "script-column.hh"

/** 
  struct to treat a set of rests as union (one voicegroup should
  only produce one rest.
  */
class Rest_column : public Script_column {
    Link_array<Note_head> head_l_arr_;
public:
    int dir_i_;
    void add(Note_head *);
    NAME_MEMBERS();
    void translate_y(Real dy);
    Rest_column();
protected:
    virtual void do_substitute_dependency(Score_elem*, Score_elem*);
};

#endif // REST_COLUMN_HH
