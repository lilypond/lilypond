/*
  book.cc -- implement Book

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>

#include "ly-smobs.icc"

#include "book.hh"
#include "global-context.hh"
#include "ly-module.hh"
#include "main.hh"
#include "music-iterator.hh"
#include "music-output-def.hh"
#include "music-output.hh"
#include "music.hh"
#include "page.hh"
#include "paper-book.hh"
#include "paper-def.hh"
#include "score.hh"
#include "warn.hh"

Book::Book ()
  : Input ()
{
  header_ = SCM_EOL;
  smobify_self ();
}

#if 0
Book::Book (Book const &src)
  : Input (src)
{
  header_ = SCM_EOL;
  smobify_self ();

  int score_count = src.scores_.size ();
  for (int i = 0; i < score_count; i++)
    scores_.push (src.scores_[i]->clone ());

#if 0
  header_ = ly_make_anonymous_module ();
  if (is_module (src.header_))
    ly_import_module (header_, src.header_);
#endif
}
#endif

Book::~Book ()
{
}

IMPLEMENT_SMOBS (Book);
IMPLEMENT_DEFAULT_EQUAL_P (Book);

SCM
Book::mark_smob (SCM s)
{
  Book *book = (Book*) SCM_CELL_WORD_1 (s);
  int score_count = book->scores_.size ();
  for (int i = 0; i < score_count; i++)
    scm_gc_mark (book->scores_[i]->self_scm ());
  return book->header_;
}

int
Book::print_smob (SCM, SCM p, scm_print_state*)
{
  scm_puts ("#<Book>", p);
  return 1;
}

void
Book::process (String outname, Music_output_def *default_def, SCM header)
{
  Paper_book *paper_book = new Paper_book ();
  int score_count = scores_.size ();
  for (int i = 0; i < score_count; i++)
    {
      Paper_def *paper = 0;
      SCM systems = scores_[i]->book_rendering (outname, default_def, &paper);
      if (systems != SCM_UNDEFINED)
	{
	  if (paper)
	    paper_book->papers_.push (paper);
	  paper_book->scores_.push (systems);

	  // fixme.
	  //paper_book->global_headers_.push (global_input_file->header_);
	  //paper_book->headers_.push (scores_[i]->header_);
	  paper_book->headers_.push (header);
	}
    }
  paper_book->output (outname);
  scm_gc_unprotect_object (paper_book->self_scm ());
}

/* FIXME: WIP, this is a hack.  Return first page as stencil.  */
SCM
Book::to_stencil (Music_output_def *default_def, SCM header)
{
  Paper_book *paper_book = new Paper_book ();
  int score_count = scores_.size ();
  for (int i = 0; i < score_count; i++)
    {
      Paper_def *paper = 0;
      SCM systems = scores_[i]->book_rendering ("", default_def, &paper);
      if (systems != SCM_UNDEFINED)
	{
	  if (paper)
	    paper_book->papers_.push (paper);
	  paper_book->scores_.push (systems);
	  paper_book->headers_.push (header);
	}
    }

  SCM pages = paper_book->pages ();
  scm_gc_unprotect_object (paper_book->self_scm ());
  if (pages != SCM_EOL)
    return unsmob_page (ly_car (pages))->to_stencil ();
  return SCM_EOL;
}
