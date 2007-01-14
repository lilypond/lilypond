/*
  paper-book-scheme.cc -- implement Paper_book bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "paper-book.hh"
#include "ly-module.hh"
#include "output-def.hh"

LY_DEFINE (ly_paper_book_pages, "ly:paper-book-pages",
	   1, 0, 0, (SCM pb),
	   "Return pages in book PB.")
{
  return unsmob_paper_book (pb)->pages ();
}

LY_DEFINE (ly_paper_book_scopes, "ly:paper-book-scopes",
	   1, 0, 0, (SCM book),
	   "Return pages in layout book @var{book}.")
{
  Paper_book *pb = unsmob_paper_book (book);
  SCM_ASSERT_TYPE (pb, book, SCM_ARG1, __FUNCTION__, "Paper_book");

  SCM scopes = SCM_EOL;
  if (ly_is_module (pb->header_))
    scopes = scm_cons (pb->header_, scopes);

  return scopes;
}

LY_DEFINE (ly_paper_book_performances, "ly:paper-book-performances",
	   1, 0, 0, (SCM paper_book),
	   "Return performances in book @var{paper-book}.")
{
  return unsmob_paper_book (paper_book)->performances ();
}

LY_DEFINE (ly_paper_book_systems, "ly:paper-book-systems",
	   1, 0, 0, (SCM pb),
	   "Return systems in book PB.")
{
  return unsmob_paper_book (pb)->systems ();
}

LY_DEFINE (ly_paper_book_paper, "ly:paper-book-paper",
	   1, 0, 0, (SCM pb),
	   "Return pages in book PB.")
{
  Paper_book *pbook = unsmob_paper_book (pb);
  SCM_ASSERT_TYPE(pbook, pb, SCM_ARG1, __FUNCTION__, "Paper_book");
  return pbook->paper_->self_scm ();
}
