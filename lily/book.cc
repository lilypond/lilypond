/*
  book.cc -- implement Book

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>

#include "book.hh"
#include "global-context.hh"
#include "ly-module.hh"
#include "main.hh"
#include "music-iterator.hh"
#include "music-output.hh"
#include "music.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "score.hh"
#include "stencil.hh"
#include "warn.hh"

#include "ly-smobs.icc"

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

/* This function does not dump the output; outname is required eg. for
   dumping header fields.  */
Paper_book *
Book::process (String outname, Output_def *default_def)
{
  bool error = false;
  for (int i = 0; i < scores_.size(); i++)
    error = error || scores_[i]->error_found_;

  if (error)
    return 0;
    
  Paper_book *paper_book = new Paper_book ();
  Real scale = scm_to_double (bookpaper_->c_variable ("outputscale"));
  
  Output_def * scaled_bookdef = scale_output_def (bookpaper_, scale);

  paper_book->bookpaper_ = scaled_bookdef;
  scm_gc_unprotect_object (scaled_bookdef->self_scm());
  
  paper_book->header_ = header_;
  
  int score_count = scores_.size ();
  for (int i = 0; i < score_count; i++)
    {
      SCM systems = scores_[i]->book_rendering (outname,
						paper_book->bookpaper_,
						default_def);
      
      /* If the score is empty, generate no output.  Should we
	 do titling?  */
      if (SCM_NFALSEP(scm_vector_p (systems)))
	{
	  Score_systems sc;
	  sc.systems_ = systems;
	  sc.header_ = scores_[i]->header_;
	  paper_book->score_systems_.push (sc);
	}
    }

  return paper_book;
}

LY_DEFINE(ly_make_book, "ly:make-book",
	  2, 0, 1, (SCM bookpaper, SCM header, SCM scores),
	  "Make a \\book of @var{bookpaper} and @var{header} (which may be #f as well)  "
	  "containing @code{\\scores}.")
{
  Output_def * odef = unsmob_output_def (bookpaper);
  SCM_ASSERT_TYPE (odef, bookpaper,
		   SCM_ARG1, __FUNCTION__, "Output_def");

  Book *book = new Book;
  book->bookpaper_ = odef;

  if (ly_c_module_p (header))
    book->header_ = header;
  
  for (SCM s = scores; scm_is_pair (s); s = ly_cdr (s))
    {
      Score *score = unsmob_score (ly_car (s));
      if (score)
	book->scores_.push (score);
    }
  
  SCM x = book->self_scm ();
  scm_gc_unprotect_object (x);
  return x;
}
  
