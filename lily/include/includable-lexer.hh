/*
  includable-lexer.hh -- declare Includable_lexer

  source file of the LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef INCLUDABLE_LEXER_HH
#define INCLUDABLE_LEXER_HH

#include <FlexLexer.h>

#include "string.hh"
#include "parray.hh"
#include "lily-proto.hh"

// GIGA urg!
typedef struct yy_buffer_state *YY_BUFFER_STATE;

/**
  an yyFlexLexer child with provisions for inclusion.
 */
class Includable_lexer : public yyFlexLexer 
{
  Array<YY_BUFFER_STATE> state_stack_;

protected:
  bool  close_input ();
  Link_array<Source_file> include_stack_;
  Array<int> char_count_stack_;

public:
  bool allow_includes_b_;
  
  Includable_lexer ();
  ~Includable_lexer ();

  /// store dependencies for Makefile stuff.
  Array<String> filename_str_arr_;

  Source_file* source_file_l () const;
  void new_input (String s, Sources*);
  void new_input (String name, String data, Sources*);

  void add_lexed_char (int);
  char const * here_ch_C () const;
};


#endif // INCLUDABLE_LEXER_HH
