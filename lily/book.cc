/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "main.hh"
#include "music.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "score.hh"
#include "text-interface.hh"
#include "time-tracer.hh"
#include "warn.hh"
#include "performance.hh"
#include "paper-score.hh"
#include "page-marker.hh"
#include "ly-module.hh"

#include <cstdio>
#include <string_view>

using namespace std::literals;

Book_or_bookpart::Book_or_bookpart ()
{
  header_ = SCM_BOOL_F;
  scores_ = SCM_EOL;
  input_location_ = SCM_UNDEFINED;
  scope_module_ = SCM_UNDEFINED;
  smobify_self ();
  input_location_ = Input ().smobbed_copy ();
  scope_module_ = ly_make_module ();
}

Book_or_bookpart::Book_or_bookpart (Book_or_bookpart const &s)
{
  header_ = SCM_BOOL_F;
  scores_ = SCM_EOL;
  input_location_ = SCM_UNDEFINED;
  scope_module_ = SCM_UNDEFINED;
  smobify_self ();
  input_location_ = s.origin ()->smobbed_copy ();

  scope_module_ = ly_make_module ();
  ly_module_copy (scope_module_, s.scope_module_);

  if (ly_is_module (s.header_)) {
    header_ = ly_make_module ();
    ly_module_copy (header_, s.header_);
  }
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
}

Book::Book (Book const &s) : Book_or_bookpart (s)
{
  SCM *t = &bookparts_;
  for (SCM p = s.bookparts_; scm_is_pair (p); p = scm_cdr (p))
    {
      Bookpart *newpart = unsmob<Bookpart> (scm_car (p))->clone ();

      *t = scm_cons (newpart->self_scm (), SCM_EOL);
      t = SCM_CDRLOC (*t);
      newpart->unprotect ();
    }
}

Input *
Book_or_bookpart::origin () const
{
  return unsmob<Input> (input_location_);
}

Book_or_bookpart::~Book_or_bookpart ()
{
}

Book::~Book ()
{
}

Bookpart::~Bookpart ()
{
}

SCM
Book_or_bookpart::mark_smob () const
{
  derived_mark ();
  scm_gc_mark (scores_);
  scm_gc_mark (input_location_);
  scm_gc_mark (scope_module_);

  return header_;
}

int
Book_or_bookpart::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<", port);
  scm_puts (class_name (), port);
  scm_puts (">", port);
  return 1;
}

void
Book::derived_mark () const
{
  scm_gc_mark (bookparts_);
}

void
Book_or_bookpart::add_score (SCM s)
{
  scores_ = scm_cons (s, scores_);
}

void
Bookpart::set_parent (Book *parent)
{
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
      Bookpart *part = new Bookpart;
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
  Bookpart *part = unsmob<Bookpart> (b);
  part->set_parent (this);
  bookparts_ = scm_cons (b, bookparts_);
}

bool
Book_or_bookpart::error_found () const
{
  for (auto *score : as_ly_smob_list<const Score> (scores_))
    {
      if (score && score->error_found_)
        return true;
    }
  return false;
}

bool
Book::error_found () const
{
  if (Book_or_bookpart::error_found ())
    return true;

  for (auto *bookpart : as_ly_smob_list<const Bookpart> (bookparts_))
    {
      if (bookpart && bookpart->error_found ())
        return true;
    }

  return false;
}

void
Book::process_bookparts (Paper_book *output_paper_book, Output_def *paper,
                         Output_def *layout)
{
  add_scores_to_bookpart ();
  for (auto *bookpart : ly_smob_list<Bookpart> (scm_reverse (bookparts_)))
    {
      if (bookpart)
        {
          auto trace_slice = tracer_global.log_scope ("bookpart"sv);
          Paper_book *paper_book_part
            = bookpart->process (paper, layout, output_paper_book);
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
Book_or_bookpart::process_score (SCM score_scm, Paper_book *output_paper_book,
                          Output_def *layout)
{
  if (Score *score = unsmob<Score> (score_scm))
    {
      auto trace_slice = tracer_global.log_scope ("score"sv);
      SCM outputs
        = score->book_rendering (output_paper_book->paper (), layout);

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

// This happens rarely enough that we don't need to cache it.

Output_def *
Book_or_bookpart::paper () const
{
  SCM scm_paper = scm_module_variable (scope (),
                                       ly_symbol2scm ("$defaultpaper"));
  if (scm_is_true (scm_paper))
    {
      return unsmob<Output_def> (scm_variable_ref (scm_paper));
    }
  return nullptr;
}

void
Book::set_paper (SCM paper)
{
  scm_module_define (scope (), ly_symbol2scm ("$defaultpaper"), paper);
}

Output_def *
Book_or_bookpart::layout () const
{
  SCM scm_layout = scm_module_variable (scope (),
                                       ly_symbol2scm ("$defaultlayout"));
  if (scm_is_true (scm_layout))
    {
      return unsmob<Output_def> (scm_variable_ref (scm_layout));
    }
  return nullptr;
}

// No need for set_layout () since \paper is the only kind of output definition
// that always gets set in a book.

/* Concatenate all score or book part outputs into a Paper_book
 */

Paper_book *
Book::process (Output_def *default_paper, Output_def *default_layout)
{
  auto trace_slice = tracer_global.log_scope ("Process book"sv);
  Output_def *paper = Book::paper ();

  if (!paper)
    paper = default_paper;

  Output_def *layout = Book::layout ();

  if (!layout)
    layout = default_layout;

  if (!paper || !layout)
    return nullptr;

  /* top book, recursively check score errors */
  if (error_found ())
    return nullptr;

  Paper_book *paper_book = new Paper_book (paper, 0);
  paper_book->header_ = header_;

  if (scm_is_pair (bookparts_))
    {
      /* Process children book parts */
      process_bookparts (paper_book, paper, layout);
    }
  else
    {
      paper_book->paper ()->normalize ();
      /* Process scores */
      /* Render in order of parsing.  */
      for (SCM s = scm_reverse (scores_); scm_is_pair (s); s = scm_cdr (s))
        {
          process_score (scm_car (s), paper_book, layout);
        }
    }

  return paper_book;
}

Paper_book *
Bookpart::process (Output_def *default_paper, Output_def *default_layout,
                   Paper_book *parent_part)
{
  Output_def *paper = Bookpart::paper ();

  if (!paper)
    paper = default_paper;

  Output_def *layout = Bookpart::layout ();

  if (!layout)
    layout = default_layout;

  if (!paper || !layout)
    return nullptr;

  Paper_book *paper_book = new Paper_book (paper, parent_part);
  paper_book->header_ = header_;

  paper_book->paper ()->normalize ();
  /* Process scores */
  /* Render in order of parsing.  */
  for (SCM s = scm_reverse (scores_); scm_is_pair (s); s = scm_cdr (s))
    {
      process_score (scm_car (s), paper_book, layout);
    }
  return paper_book;
}
