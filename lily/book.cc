/*
  book.cc -- implement Book

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>

#include "book-paper-def.hh"
#include "ly-smobs.icc"
#include "stencil.hh"
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
  bookpaper_ = 0;
  header_ = SCM_EOL;
  assert (!scores_.size ());
  smobify_self ();
}

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

  if (book->bookpaper_)
    scm_gc_mark (book->bookpaper_->self_scm ());
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
  paper_book->bookpaper_ = bookpaper_;
  int score_count = scores_.size ();
  for (int i = 0; i < score_count; i++)
    {
      Paper_def *paper = 0;
      SCM systems = scores_[i]->book_rendering (outname,
						bookpaper_,
						default_def, &paper);
      if (systems != SCM_UNDEFINED)
	{
	  Score_lines sc;
	  sc.paper_ = paper;
	  sc.lines_ = systems;
	  sc.header_ = header;

	  paper_book->score_lines_.push (sc);
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
  paper_book->bookpaper_ = bookpaper_;
  int score_count = scores_.size ();
  for (int i = 0; i < score_count; i++)
    {
      Paper_def *paper = 0;
      SCM systems = scores_[i]->book_rendering ("<markup>",
						bookpaper_,
						default_def,
						&paper);
      if (systems != SCM_UNDEFINED)
	{
	  Score_lines sc;
	  sc.paper_ = paper;
	  sc.lines_ = systems;
	  sc.header_ =header;

	  paper_book->score_lines_.push (sc);

	  // wtf: code dup.
	}
    }

  SCM pages = paper_book->pages ();
  paper_book = 0;
  if (pages != SCM_EOL)
    {
      progress_indication (_f ("paper output to `%s'...", "<markup>"));
      return (unsmob_page (ly_car (pages)))->to_stencil ().smobbed_copy ();
    }
  return SCM_EOL;
}
