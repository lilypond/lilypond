/*
  rest-column.hh -- declare Rest_column

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef REST_COLUMN_HH
#define REST_COLUMN_HH

#include "script-column.hh"
#include "head-column.hh"

/** 
  struct to treat a set of rests as union (one voicegroup should
  only produce one rest.
  */
class Rest_column : public Head_column {
public:
    NAME_MEMBERS();
    void translate_heads(int dy);
};

#endif // REST_COLUMN_HH
