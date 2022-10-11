/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "ly-smob-list.hh"
#include "music.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "score.hh"
#include "text-interface.hh"
#include "warn.hh"
#include "performance.hh"
#include "paper-score.hh"
#include "page-marker.hh"
#include "ly-module.hh"

#include <cstdio>

Book::Book ()
{
  paper_ = 0;
  header_ = SCM_EOL;
  scores_ = SCM_EOL;
  bookparts_ = SCM_EOL;
  input_location_ = SCM_EOL;
  smobify_self ();

  input_location_ = Input ().smobbed_copy ();
}

Book::Book (Book const &s)
  : Smob<Book> ()
{
  paper_ = 0;
  header_ = SCM_EOL;
  scores_ = SCM_EOL;
  bookparts_ = SCM_EOL;
  input_location_ = SCM_EOL;
  smobify_self ();

  if (s.paper_)
    {
      paper_ = s.paper_->clone ();
      paper_->unprotect ();
    }

  input_location_ = s.origin ()->smobbed_copy ();

  header_ = ly_make_module ();
  if (ly_is_module (s.header_))
    ly_module_copy (header_, s.header_);
  SCM *t = &scores_;
  for (SCM p = s.scores_; scm_is_pair (p); p = scm_cdr (p))
    {
      SCM entry = scm_car (p);

      if (Score *newscore = unsmob<Score> (entry))
        *t = scm_cons (newscore->clone ()->unprotect (), SCM_EOL);
      else if (Page_marker *marker = unsmob<Page_marker> (entry))
        *t = scm_cons (marker->clone ()->unprotect (), SCM_EOL);
      else
        {
          /* This entry is a markup list */
          *t = scm_cons (entry, SCM_EOL);
        }
      t = SCM_CDRLOC (*t);
    }

  t = &bookparts_;
  for (SCM p = s.bookparts_; scm_is_pair (p); p = scm_cdr (p))
    {
      Book *newpart = unsmob<Book> (scm_car (p))->clone ();

      *t = scm_cons (newpart->self_scm (), SCM_EOL);
      t = SCM_CDRLOC (*t);
      newpart->unprotect ();
    }
}

Input *
Book::origin () const
{
  return unsmob<Input> (input_location_);
}

Book::~Book ()
{
}

SCM
Book::mark_smob () const
{
  if (paper_)
    scm_gc_mark (paper_->self_scm ());
  scm_gc_mark (scores_);
  scm_gc_mark (bookparts_);
  scm_gc_mark (input_location_);

  return header_;
}

void
Book::add_score (SCM s)
{
  scores_ = scm_cons (s, scores_);
}

void
Book::set_parent (Book *parent)
{
  if (!paper_)
    {
      paper_ = new Output_def ();
      paper_->unprotect ();
    }
  paper_->parent_ = parent->paper_;
  /* Copy the header block of the parent */
  if (ly_is_module (parent->header_))
    {
      SCM tmp_header = ly_make_module ();
      ly_module_copy (tmp_header, parent->header_);
      if (ly_is_module (header_))
        ly_module_copy (tmp_header, header_);
      header_ = tmp_header;
    }
}

/* Before an explicit \bookpart is encountered, scores are added to the book.
 * But once a bookpart is added, the previous scores shall be collected into
 * a new bookpart.
 */
void
Book::add_scores_to_bookpart ()
{
  if (scm_is_pair (scores_))
    {
      /* If scores have been added to this book, add them to a child
       * book part */
      Book *part = new Book;
      part->set_parent (this);
      part->scores_ = scores_;
      bookparts_ = scm_cons (part->self_scm (), bookparts_);
      part->unprotect ();
      scores_ = SCM_EOL;
    }
}

void
Book::add_bookpart (SCM b)
{
  add_scores_to_bookpart ();
  Book *part = unsmob<Book> (b);
  part->set_parent (this);
  bookparts_ = scm_cons (b, bookparts_);
}

bool
Book::error_found () const
{
  for (auto *score : as_ly_smob_list<const Score> (scores_))
    {
      if (score && score->error_found_)
        return true;
    }

  for (auto *bookpart : as_ly_smob_list<const Book> (bookparts_))
    {
      if (bookpart && bookpart->error_found ())
        return true;
    }

  return false;
}

Paper_book *
Book::process (Output_def *default_paper, Output_def *default_layout)
{
  return process (default_paper, default_layout, 0);
}

void
Book::process_bookparts (Paper_book *output_paper_book, Output_def *paper,
                         Output_def *layout)
{
  add_scores_to_bookpart ();
  for (auto *book : ly_smob_list<Book> (scm_reverse (bookparts_)))
    {
      if (book)
        {
          Paper_book *paper_book_part
            = book->process (paper, layout, output_paper_book);
          if (paper_book_part)
            {
              output_paper_book->add_bookpart (paper_book_part->self_scm ());
              paper_book_part->unprotect ();
            }
        }
    }
}

/* process one entry of scores_ */
void
Book::process_score (SCM score_scm, Paper_book *output_paper_book,
                     Output_def *layout)
{
  if (Score *score = unsmob<Score> (score_scm))
    {
      SCM outputs = score->book_rendering (output_paper_book->paper (), layout);

      while (scm_is_pair (outputs))
        {
          Music_output *output = unsmob<Music_output> (scm_car (outputs));

          if (Performance *perf = dynamic_cast<Performance *> (output))
            {
              output_paper_book->add_performance (perf->self_scm ());

              // Collect the \header blocks to make the metadata accessible
              // when outputting the performance.
              SCM h = output_paper_book->header_0_;
              if (ly_is_module (h))
                perf->push_header (h);

              h = output_paper_book->header_;
              if (ly_is_module (h))
                perf->push_header (h);

              h = score->get_header ();
              if (ly_is_module (h))
                perf->push_header (h);
            }
          else if (Paper_score *pscore = dynamic_cast<Paper_score *> (output))
            {
              if (ly_is_module (score->get_header ()))
                output_paper_book->add_score (score->get_header ());
              output_paper_book->add_score (pscore->self_scm ());
            }

          outputs = scm_cdr (outputs);
        }
    }
  else if (Text_interface::is_markup_list (score_scm)
           || unsmob<Page_marker> (score_scm))
    output_paper_book->add_score (score_scm);
  else
    assert (0);

  scm_remember_upto_here_1 (score_scm);
}

/* Concatenate all score or book part outputs into a Paper_book
 */
Paper_book *
Book::process (Output_def *default_paper, Output_def *default_layout,
               Paper_book *parent_part)
{
  Output_def *paper = paper_ ? paper_ : default_paper;

  /* If top book, recursively check score errors */
  if (!parent_part && error_found ())
    return 0;

  if (!paper)
    return 0;

  Paper_book *paper_book = new Paper_book (paper, parent_part);
  paper_book->header_ = header_;

  if (scm_is_pair (bookparts_))
    {
      /* Process children book parts */
      process_bookparts (paper_book, paper, default_layout);
    }
  else
    {
      paper_book->paper ()->normalize ();
      /* Process scores */
      /* Render in order of parsing.  */
      for (SCM s = scm_reverse (scores_); scm_is_pair (s); s = scm_cdr (s))
        {
          process_score (scm_car (s), paper_book, default_layout);
        }
    }

  return paper_book;
}
