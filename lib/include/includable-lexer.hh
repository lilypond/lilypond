/*
  includable-lexer.hh -- declare Includable_lexer

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef INCLUDABLE_LEXER_HH
#define INCLUDABLE_LEXER_HH

#include <FlexLexer.h>

#include "string.hh"
#include "varray.hh"
#include "fproto.hh"
#include "proto.hh"

// GIGA urg!
typedef struct yy_buffer_state *YY_BUFFER_STATE;

/**
  an yyFlexLexer child with provisions for inclusion.
 */
class Includable_lexer : public yyFlexLexer {
  Array<YY_BUFFER_STATE> state_stack_;
protected:
  bool  close_input ();
  Array<Source_file*> include_stack_;
  Array<int> char_count_stack_;
public:

  Source_file* source_file_l () const;
  void new_input (String s,Sources*);
  Includable_lexer ();
  ~Includable_lexer ();
  void add_lexed_char (int);
  char const * here_ch_C ();
};


#endif // INCLUDABLE_LEXER_HH
