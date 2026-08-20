/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "book.hh"

#include "output-def.hh"
#include "score.hh"
#include "paper-book.hh"

LY_DEFINE (ly_make_book, "ly:make-book", 2, 0, 1,
           (SCM paper, SCM header, SCM scores),
           R"(
Make a @code{\book} of @var{paper} and @var{header} (which may be @code{#f} as
well) containing @code{\score}s.
           )")
{
  LY_ASSERT_SMOB (Output_def, paper, 1);

  Book *book = new Book;
  book->set_paper (paper);

  if (ly_is_module (header))
    book->header_ = header;

  book->scores_ = ly_append (scores, book->scores_);

  return book->unprotect ();
}

LY_DEFINE (ly_make_book_part, "ly:make-book-part", 1, 0, 0, (SCM scores),
           R"(
Make a @code{\bookpart} containing @code{\score}s.
           )")
{
  Bookpart *bookpart = new Bookpart;
  bookpart->scores_ = ly_append (scores, bookpart->scores_);

  return bookpart->unprotect ();
}

LY_DEFINE (ly_book_process, "ly:book-process", 4, 0, 0,
           (SCM book_smob, SCM default_paper, SCM default_layout, SCM output),
           R"(
Print book.  @var{output} is passed to the backend unchanged.  For example, it
may be a string (for file based outputs) or a socket (for network based
output).
           )")
{
  auto *const book = LY_ASSERT_SMOB (Book, book_smob, 1);
  auto *const paper = LY_ASSERT_SMOB (Output_def, default_paper, 2);
  auto *const layout = LY_ASSERT_SMOB (Output_def, default_layout, 3);

  Paper_book *pb = book->process (paper, layout);

  // Make sure the Scheme objects are not collected while processing.
  scm_remember_upto_here_1 (book_smob);
  scm_remember_upto_here_2 (default_paper, default_layout);

  if (pb)
    {
      pb->output (output);
      pb->unprotect ();
    }

  return SCM_UNSPECIFIED;
}

// Urgh: Fix documentation.  Right now, it is the same as `ly:book-process`.
LY_DEFINE (ly_book_process_to_systems, "ly:book-process-to-systems", 4, 0, 0,
           (SCM book_smob, SCM default_paper, SCM default_layout, SCM output),
           R"(
Print book.  @var{output} is passed to the backend unchanged.  For example, it
may be a string (for file based outputs) or a socket (for network based
output).
           )")
{
  auto *const book = LY_ASSERT_SMOB (Book, book_smob, 1);
  auto *const paper = LY_ASSERT_SMOB (Output_def, default_paper, 2);
  auto *const layout = LY_ASSERT_SMOB (Output_def, default_layout, 3);

  Paper_book *pb = book->process (paper, layout);

  scm_remember_upto_here_1 (book_smob);
  scm_remember_upto_here_2 (default_paper, default_layout);

  if (pb)
    {
      pb->classic_output (output);
      pb->unprotect ();
    }

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_book_add_score_x, "ly:book-add-score!", 2, 0, 0,
           (SCM book_or_bookpart, SCM score),
           R"(
Add @var{score} to @var{book-or-bookpart} score list.
           )")
{
  auto *const b = LY_ASSERT_SMOB (Book_or_bookpart, book_or_bookpart, 1);
  b->add_score (score);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_book_add_bookpart_x, "ly:book-add-bookpart!", 2, 0, 0,
           (SCM book, SCM book_part),
           R"(
Add @var{book-part} to @var{book} book part list.
           )")
{
  auto *const b = LY_ASSERT_SMOB (Book, book, 1);
  LY_ASSERT_SMOB (Bookpart, book_part, 2);
  b->add_bookpart (book_part);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_book_book_parts, "ly:book-book-parts", 1, 0, 0, (SCM book),
           R"(
Return book parts in @var{book}.
           )")
{
  auto *const b = LY_ASSERT_SMOB (Book, book, 1);
  return b->bookparts_;
}

LY_DEFINE (ly_book_paper, "ly:book-paper", 1, 0, 0, (SCM book_or_bookpart),
           R"(
Return paper in @var{book_or_bookpart}.
           )")
{
  auto *const b = LY_ASSERT_SMOB (Book_or_bookpart, book_or_bookpart, 1);
  if (Output_def const *od = b->paper ())
    return od->self_scm ();
  return SCM_BOOL_F;
}

LY_DEFINE (ly_book_scope, "ly:book-scope", 1, 0, 0, (SCM book_or_bookpart),
           R"(
Return the module containing the variables local to @var{book_or_bookpart}.
           )")
{
  auto *const b = LY_ASSERT_SMOB (Book_or_bookpart, book_or_bookpart, 1);
  return b->scope ();
}

LY_DEFINE (ly_book_set_variable_x, "ly:book-set-variable!", 3, 0, 0,
           (SCM book_or_bookpart, SCM symbol, SCM value),
           R"(
In the local variables of @var{book_or_bookpart}, set the variable given by
@var{symbol} to @var{value}.
           )")
{
  auto *const b = LY_ASSERT_SMOB (Book_or_bookpart, book_or_bookpart, 1);
  LY_ASSERT_TYPE (ly_is_symbol, symbol, 2);
  scm_module_define (b->scope_module (), symbol, value);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_book_lookup, "ly:book-lookup", 2, 1, 0,
           (SCM book_or_bookpart, SCM symbol, SCM fallback),
           R"(
Look up the variable @var{symbol} in @var{book_or_bookpart}
and return its value.
If it is undefined, return @var{fallback} if given, else '().
           )")
{
  auto *const b = LY_ASSERT_SMOB (Book_or_bookpart, book_or_bookpart, 1);
  LY_ASSERT_TYPE (ly_is_symbol, symbol, 2);
  SCM v = scm_module_variable (b->scope (), symbol);
  if (SCM_VARIABLEP (v)) {
    SCM res = SCM_VARIABLE_REF (v);
    if (!SCM_UNBNDP (res))
      return res;
  }
  return SCM_UNBNDP (fallback) ? SCM_EOL : fallback;
}

LY_DEFINE (ly_book_header, "ly:book-header", 1, 0, 0, (SCM book_or_bookpart),
           R"(
Return header in @var{book_or_bookpart}.
           )")
{
  auto *const b = LY_ASSERT_SMOB (Book_or_bookpart, book_or_bookpart, 1);
  return ly_is_module (b->header_) ? b->header_ : SCM_BOOL_F;
}

LY_DEFINE (ly_book_set_header_x, "ly:book-set-header!", 2, 0, 0,
           (SCM book_or_bookpart, SCM module),
           R"(
Set the book header.
           )")
{
  auto *const b = LY_ASSERT_SMOB (Book_or_bookpart, book_or_bookpart, 1);
  SCM_ASSERT_TYPE (ly_is_module (module), module, SCM_ARG2, __FUNCTION__,
                   "module");

  b->header_ = (module);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_book_scores, "ly:book-scores", 1, 0, 0, (SCM book_or_bookpart),
           R"(
Return scores in @var{book_or_bookpart}.
           )")
{
  auto *const b = LY_ASSERT_SMOB (Book_or_bookpart, book_or_bookpart, 1);
  return b->scores_;
}

LY_DEFINE (ly_book_p, "ly:book?", 1, 0, 0, (SCM book),
           R"(
Is @var{book} a book?
           )")
{
  return to_scm (static_cast<bool> (unsmob<Book> (book)));
}

LY_DEFINE (ly_bookpart_p, "ly:bookpart?", 1, 0, 0, (SCM bookpart),
           R"(
Is @var{bookpart} a bookpart?
           )")
{
  return to_scm (static_cast<bool> (unsmob<Bookpart> (bookpart)));
}

const char *const Book_or_bookpart::type_p_name_ = "ly:book-or-bookpart?";
