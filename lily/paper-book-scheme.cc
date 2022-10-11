/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "output-def.hh"

LY_DEFINE (ly_paper_book_pages, "ly:paper-book-pages", 1, 0, 0, (SCM pb),
           R"(
Return pages in @code{Paper_book} object @var{pb}.
           )")
{
  auto *const pbook = LY_ASSERT_SMOB (Paper_book, pb, 1);
  return pbook->pages ();
}

LY_DEFINE (ly_paper_book_scopes, "ly:paper-book-scopes", 1, 0, 0, (SCM pb),
           R"(
Return scopes in @code{Paper_book} object @var{pb}.
           )")
{
  auto *const book = LY_ASSERT_SMOB (Paper_book, pb, 1);

  return book->get_scopes ();
}

LY_DEFINE (ly_paper_book_performances, "ly:paper-book-performances", 1, 0, 0,
           (SCM pb),
           R"(
Return performances in @code{Paper_book} object @var{pb}.
           )")
{
  auto *const pbook = LY_ASSERT_SMOB (Paper_book, pb, 1);
  return pbook->performances ();
}

LY_DEFINE (ly_paper_book_systems, "ly:paper-book-systems", 1, 0, 0, (SCM pb),
           R"(
Return systems in @code{Paper_book} object @var{pb}.
           )")
{
  auto *const pbook = LY_ASSERT_SMOB (Paper_book, pb, 1);
  return pbook->systems ();
}

LY_DEFINE (ly_paper_book_paper, "ly:paper-book-paper", 1, 0, 0, (SCM pb),
           R"(
Return the paper output definition (@code{\paper}) in @code{Paper_book} object
@var{pb}.
           )")
{
  auto *const pbook = LY_ASSERT_SMOB (Paper_book, pb, 1);
  return pbook->paper ()->self_scm ();
}

LY_DEFINE (ly_paper_book_header, "ly:paper-book-header", 1, 0, 0, (SCM pb),
           R"(
Return the header definition (@code{\header}) in @code{Paper_book} object
@var{pb}.
           )")
{
  auto *const pbook = LY_ASSERT_SMOB (Paper_book, pb, 1);
  return pbook->header_;
}
