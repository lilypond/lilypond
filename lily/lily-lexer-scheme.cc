/* 
  lily-lexer-scheme.cc -- implement Lily_lexer bindings. 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2006--2008 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "lily-lexer.hh"

LY_DEFINE (ly_lexer_keywords, "ly:lexer-keywords",
	  1, 0, 0, (SCM lexer),
	  "Return a list of @code{(KEY . CODE)} pairs, signifying the"
	  " LilyPond reserved words list.")
{
  LY_ASSERT_SMOB (Lily_lexer, lexer, 1);

  Lily_lexer * lex = Lily_lexer::unsmob (lexer);
  return lex->keyword_list ();
}
