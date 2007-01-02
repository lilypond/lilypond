/* 
  lily-lexer-scheme.cc -- implement Lily_lexer bindings. 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2006 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "lily-lexer.hh"

LY_DEFINE(ly_lexer_keywords, "ly:lexer-keywords",
	  1, 0, 0, (SCM lexer),
	  "Return a list of (KEY . CODE) pairs, signifying the lilypond reserved words list.")
{
  Lily_lexer * lex = Lily_lexer::unsmob (lexer);
  SCM_ASSERT_TYPE(lex, lexer, SCM_ARG1, __FUNCTION__, "lily lexer");
  return lex->keyword_list ();
}
	  

LY_DEFINE(ly_lexer_set_safe, "ly:lexer-set-safe!",
	  1, 0, 0, (SCM lexer),
	  "Switch on safe mode for the rest of the file.")
{
  Lily_lexer * lex = Lily_lexer::unsmob (lexer);
  SCM_ASSERT_TYPE(lex, lexer, SCM_ARG1, __FUNCTION__, "lily lexer");

  lex->be_safe_ = true;
  lex->allow_includes_ = false;
  
  return SCM_UNSPECIFIED;
}
