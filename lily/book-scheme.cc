/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

LY_DEFINE (ly_make_book, "ly:make-book",
           2, 0, 1, (SCM paper, SCM header, SCM scores),
           "Make a @code{\\book} of @var{paper} and @var{header}"
           " (which may be @code{#f} as well) containing @code{\\scores}.")
{
  Output_def *odef = unsmob<Output_def> (paper);
  LY_ASSERT_SMOB (Output_def, paper, 1);

  Book *book = new Book;
  book->paper_ = odef;

  if (ly_is_module (header))
    book->header_ = header;

  book->scores_ = scm_append (scm_list_2 (scores, book->scores_));

  SCM x = book->self_scm ();
  book->unprotect ();
  return x;
}

LY_DEFINE (ly_make_book_part, "ly:make-book-part",
           1, 0, 0, (SCM scores),
           "Make a @code{\\bookpart} containing @code{\\scores}.")
{
  Book *book = new Book;
  book->scores_ = scm_append (scm_list_2 (scores, book->scores_));

  SCM x = book->self_scm ();
  book->unprotect ();
  return x;
}

LY_DEFINE (ly_book_process, "ly:book-process",
           4, 0, 0, (SCM book_smob,
                     SCM default_paper,
                     SCM default_layout,
                     SCM output),
           "Print book.  @var{output} is passed to the backend unchanged."
           "  For example, it may be a string (for file based outputs)"
           " or a socket (for network based output).")
{
  Book *book = unsmob<Book> (book_smob);

  LY_ASSERT_SMOB (Book, book_smob, 1);
  LY_ASSERT_SMOB (Output_def, default_paper, 2);
  LY_ASSERT_SMOB (Output_def, default_layout, 3);

  Paper_book *pb = book->process (unsmob<Output_def> (default_paper),
                                  unsmob<Output_def> (default_layout));

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

LY_DEFINE (ly_book_process_to_systems, "ly:book-process-to-systems",
           4, 0, 0, (SCM book_smob,
                     SCM default_paper,
                     SCM default_layout,
                     SCM output),
           "Print book.  @var{output} is passed to the backend unchanged."
           "  For example, it may be a string (for file based outputs)"
           " or a socket (for network based output).")
{
  LY_ASSERT_SMOB (Book, book_smob, 1);
  LY_ASSERT_SMOB (Output_def, default_paper, 2);
  LY_ASSERT_SMOB (Output_def, default_layout, 3);

  Book *book = unsmob<Book> (book_smob);

  Paper_book *pb = book->process (unsmob<Output_def> (default_paper),
                                  unsmob<Output_def> (default_layout));
  if (pb)
    {
      pb->classic_output (output);
      pb->unprotect ();
    }

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_book_add_score_x, "ly:book-add-score!",
           2, 0, 0, (SCM book_smob, SCM score),
           "Add @var{score} to @var{book-smob} score list.")
{
  LY_ASSERT_SMOB (Book, book_smob, 1);
  Book *book = unsmob<Book> (book_smob);
  book->add_score (score);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_book_add_bookpart_x, "ly:book-add-bookpart!",
           2, 0, 0, (SCM book_smob, SCM book_part),
           "Add @var{book-part} to @var{book-smob} book part list.")
{
  LY_ASSERT_SMOB (Book, book_smob, 1);
  Book *book = unsmob<Book> (book_smob);
  book->add_bookpart (book_part);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_book_book_parts, "ly:book-book-parts",
           1, 0, 0, (SCM book),
           "Return book parts in @var{book}.")
{
  LY_ASSERT_SMOB (Book, book, 1);
  Book *b = unsmob<Book> (book);
  return b->bookparts_;
}

LY_DEFINE (ly_book_paper, "ly:book-paper",
           1, 0, 0, (SCM book),
           "Return paper in @var{book}.")
{
  LY_ASSERT_SMOB (Book, book, 1);
  Book *b = unsmob<Book> (book);
  return b->paper_ ? b->paper_->self_scm () : SCM_BOOL_F;
}

LY_DEFINE (ly_book_header, "ly:book-header",
           1, 0, 0, (SCM book),
           "Return header in @var{book}.")
{
  LY_ASSERT_SMOB (Book, book, 1);
  Book *b = unsmob<Book> (book);
  return ly_is_module (b->header_) ? b->header_ : SCM_BOOL_F;
}

LY_DEFINE (ly_book_set_header_x, "ly:book-set-header!",
           2, 0, 0, (SCM book, SCM module),
           "Set the book header.")
{
  LY_ASSERT_SMOB (Book, book, 1);
  SCM_ASSERT_TYPE (ly_is_module (module), module, SCM_ARG2, __FUNCTION__,
                   "module");

  Book *b = unsmob<Book> (book);
  b->header_ = (module);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_book_scores, "ly:book-scores",
           1, 0, 0, (SCM book),
           "Return scores in @var{book}.")
{
  LY_ASSERT_SMOB (Book, book, 1);
  Book *b = unsmob<Book> (book);
  return b->scores_;
}

const char *const Book::type_p_name_ = "ly:book?";
