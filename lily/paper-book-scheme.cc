/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
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
