/*
  book-scheme.cc -- implement Book bindings

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "book.hh"
#include "output-def.hh"
#include "score.hh"
#include "ly-module.hh"

LY_DEFINE (ly_make_book, "ly:make-book",
	   2, 0, 1, (SCM paper, SCM header, SCM scores),
	   "Make a \\book of @var{paper} and @var{header} (which may be #f as well)  "
	   "containing @code{\\scores}.")
{
  Output_def *odef = unsmob_output_def (paper);
  SCM_ASSERT_TYPE (odef, paper,
		   SCM_ARG1, __FUNCTION__, "Output_def");

  Book *book = new Book;
  book->paper_ = odef;

  if (ly_c_module_p (header))
    book->header_ = header;

  book->scores_ = scm_append (scm_list_2 (scores, book->scores_));

  SCM x = book->self_scm ();
  scm_gc_unprotect_object (x);
  return x;
}
