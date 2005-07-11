/*
  book-scheme.cc -- implement Book bindings

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "book.hh"

#include "output-def.hh"
#include "score.hh"
#include "paper-book.hh"
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

  if (ly_is_module (header))
    book->header_ = header;

  book->scores_ = scm_append (scm_list_2 (scores, book->scores_));

  SCM x = book->self_scm ();
  scm_gc_unprotect_object (x);
  return x;
}

LY_DEFINE (ly_parser_print_book, "ly:book-process",
	   4, 0, 0, (SCM book_smob,
		     SCM default_paper,
		     SCM default_layout,
		     SCM basename),
	   "Print book.")
{
  Book *book = unsmob_book (book_smob);

  SCM_ASSERT_TYPE (book, book_smob, SCM_ARG1, __FUNCTION__, "Book");
  SCM_ASSERT_TYPE (unsmob_output_def (default_paper),
		   default_layout, SCM_ARG2, __FUNCTION__, "\\paper block");
  SCM_ASSERT_TYPE (unsmob_output_def (default_layout),
		   default_layout, SCM_ARG3, __FUNCTION__, "\\layout block");
  SCM_ASSERT_TYPE (scm_is_string (basename), basename, SCM_ARG4, __FUNCTION__, "string");

  String base = ly_scm2string (basename);
  Paper_book *pb = book->process (base,
				  unsmob_output_def (default_paper),
				  unsmob_output_def (default_layout)
				  );
  if (pb)
    {
      pb->output (base);
      scm_gc_unprotect_object (pb->self_scm ());
    }

  return SCM_UNSPECIFIED;
}

