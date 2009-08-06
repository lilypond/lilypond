/*
  paper-book-scheme.cc -- implement Paper_book bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "paper-book.hh"
#include "ly-module.hh"
#include "output-def.hh"

LY_DEFINE (ly_paper_book_pages, "ly:paper-book-pages",
	   1, 0, 0, (SCM pb),
	   "Return pages in @code{Paper_book} object @var{pb}.")
{
  LY_ASSERT_SMOB (Paper_book, pb, 1);
  return unsmob_paper_book (pb)->pages ();
}

LY_DEFINE (ly_paper_book_scopes, "ly:paper-book-scopes",
	   1, 0, 0, (SCM pb),
	   "Return scopes in @code{Paper_book} object @var{pb}.")
{
  LY_ASSERT_SMOB (Paper_book, pb, 1);
  Paper_book *book = unsmob_paper_book (pb);

  SCM scopes = SCM_EOL;
  if (book->parent_)
    {
      scopes = ly_paper_book_scopes (book->parent_->self_scm ());
    }
  if (ly_is_module (book->header_))
    scopes = scm_cons (book->header_, scopes);

  return scopes;
}

LY_DEFINE (ly_paper_book_performances, "ly:paper-book-performances",
	   1, 0, 0, (SCM pb),
	   "Return performances in @code{Paper_book} object @var{pb}.")
{
  LY_ASSERT_SMOB (Paper_book, pb, 1);
  return unsmob_paper_book (pb)->performances ();
}

LY_DEFINE (ly_paper_book_systems, "ly:paper-book-systems",
	   1, 0, 0, (SCM pb),
	   "Return systems in @code{Paper_book} object @var{pb}.")
{
  LY_ASSERT_SMOB (Paper_book, pb, 1);
  return unsmob_paper_book (pb)->systems ();
}

LY_DEFINE (ly_paper_book_paper, "ly:paper-book-paper",
	   1, 0, 0, (SCM pb),
	   "Return the paper output definition (@code{\\paper})"
	   " in @code{Paper_book} object @var{pb}.")
{
  LY_ASSERT_SMOB (Paper_book, pb, 1);
  Paper_book *pbook = unsmob_paper_book (pb);
  return pbook->paper_->self_scm ();
}
