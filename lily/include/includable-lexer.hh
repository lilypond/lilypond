/*
  includable-lexer.hh -- declare Includable_lexer

  source file of the LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef INCLUDABLE_LEXER_HH
#define INCLUDABLE_LEXER_HH

#ifndef LEXER_CC
#include <FlexLexer.h>
#endif

#include "std-string.hh"
#include "std-vector.hh"
#include "lily-proto.hh"

// GIGA urg!
typedef struct yy_buffer_state *YY_BUFFER_STATE;

/**
   an yyFlexLexer child with provisions for inclusion.
*/
class Includable_lexer : public yyFlexLexer
{
  vector<YY_BUFFER_STATE> state_stack_;

protected:
  bool close_input ();
  vector<Source_file*> include_stack_;
  vector<int> char_count_stack_;

public:

  Includable_lexer ();
  ~Includable_lexer ();

  /// store dependencies for Makefile stuff.
  vector<string> file_name_strings_;

  Source_file *get_source_file () const;
  virtual void new_input (string s, Sources *);
  
  void new_input (string name, string data, Sources *);
  
  char const *here_str0 () const;
};

#endif // INCLUDABLE_LEXER_HH
