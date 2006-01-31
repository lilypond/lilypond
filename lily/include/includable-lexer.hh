/*
  includable-lexer.hh -- declare Includable_lexer

  source file of the LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef INCLUDABLE_LEXER_HH
#define INCLUDABLE_LEXER_HH

#ifndef LEXER_CC
#include <FlexLexer.h>
#endif

#include "std-string.hh"
#include "parray.hh"
#include "lily-proto.hh"

// GIGA urg!
typedef struct yy_buffer_state *YY_BUFFER_STATE;

/**
   an yyFlexLexer child with provisions for inclusion.
*/
class Includable_lexer : public yyFlexLexer
{
  std::vector<YY_BUFFER_STATE> state_stack_;

protected:
  bool close_input ();
  Link_array<Source_file> include_stack_;
  std::vector<int> char_count_stack_;

public:
  bool allow_includes_b_;

  Includable_lexer ();
  ~Includable_lexer ();

  /// store dependencies for Makefile stuff.
  std::vector<std::string> file_name_strings_;

  Source_file *get_source_file () const;
  void new_input (std::string s, Sources *);
  void new_input (std::string name, std::string data, Sources *);

  char const *here_str0 () const;
};

#endif // INCLUDABLE_LEXER_HH
