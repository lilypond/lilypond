/*
  book-scheme.cc -- implement Book bindings

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  book->unprotect ();
  return x;
}

LY_DEFINE (ly_parser_print_book, "ly:book-process",
	   4, 0, 0, (SCM book_smob,
		     SCM default_paper,
		     SCM default_layout,
		     SCM output),
	   "Print book. @var{output} is passed to the backend unchanged. "
	   "Eg. it may be "
	   "a string (for file based outputs) or a socket (for network based "
	   "output).")
{
  Book *book = unsmob_book (book_smob);

  SCM_ASSERT_TYPE (book, book_smob, SCM_ARG1, __FUNCTION__, "Book");
  SCM_ASSERT_TYPE (unsmob_output_def (default_paper),
		   default_layout, SCM_ARG2, __FUNCTION__, "\\paper block");
  SCM_ASSERT_TYPE (unsmob_output_def (default_layout),
		   default_layout, SCM_ARG3, __FUNCTION__, "\\layout block");

  Paper_book *pb = book->process (unsmob_output_def (default_paper),
				  unsmob_output_def (default_layout));
  if (pb)
    {
      pb->output (output);
      pb->unprotect ();
    }

  return SCM_UNSPECIFIED;
}

