/*
  rest-column.hh -- declare Rest_column

  source file of the LilyPond music typesetter

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
    Array<Notehead*> head_l_arr_;
public:
    int dir_i_;
    void add(Notehead *);
    NAME_MEMBERS(Rest_column);
    void translate_y(Real dy);
};

#endif // REST_COLUMN_HH
