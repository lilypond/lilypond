/*
  book.cc -- implement Book

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "book.hh"

#include <cstdio>

#include "lilypond-key.hh"
#include "global-context.hh"
#include "main.hh"
#include "music-iterator.hh"
#include "music-output.hh"
#include "music.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "score.hh"
#include "stencil.hh"
#include "text-item.hh"
#include "warn.hh"

#include "ly-smobs.icc"

Book::Book ()
  : Input ()
{
  paper_ = 0;
  header_ = SCM_EOL;
  scores_ = SCM_EOL;
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

#if 0
  if (book->key_)
    scm_gc_mark (book->key_->self_scm());
#endif

  if (book->paper_)
    scm_gc_mark (book->paper_->self_scm ());
  scm_gc_mark (book->scores_);

  return book->header_;
}

int
Book::print_smob (SCM, SCM p, scm_print_state*)
{
  scm_puts ("#<Book>", p);
  return 1;
}

void
Book::add_score (SCM s)
{
  scores_ = scm_cons (s, scores_);
}

/* This function does not dump the output; outname is required eg. for
   dumping header fields.  */
Paper_book *
Book::process (String outname, Output_def *default_def)
{
  for (SCM s = scores_; s != SCM_EOL; s = scm_cdr (s))
    if (Score *score = unsmob_score (scm_car (s)))
      if (score->error_found_)
	return 0;

  Paper_book *paper_book = new Paper_book ();
  Real scale = scm_to_double (paper_->c_variable ("outputscale"));
  Output_def * scaled_bookdef = scale_output_def (paper_, scale);

  Object_key * key = new Lilypond_general_key (0, user_key_, 0);
  SCM  scm_key = key->self_scm();
  scm_gc_unprotect_object (scm_key);
  
  paper_book->paper_ = scaled_bookdef;
  scm_gc_unprotect_object (scaled_bookdef->self_scm());
  
  paper_book->header_ = header_;

  /* Render in order of parsing.  */
  for (SCM s = scm_reverse (scores_); s != SCM_EOL; s = scm_cdr (s))
    {
      if (Score *score = unsmob_score (scm_car (s)))
	{
	  SCM systems = score
	    ->book_rendering (outname, paper_book->paper_, default_def, key);
      
	  /* If the score is empty, generate no output.  Should we do
	     titling? */
	  if (scm_is_vector (systems))
	    {
	      if (ly_c_module_p (score->header_))
		paper_book->add_score (score->header_);
	      paper_book->add_score (systems);
	    }
	}
      else if (Text_interface::markup_p (scm_car (s)))
	paper_book->add_score (scm_car (s));
      else
	assert (0);
    }

  scm_remember_upto_here_1 (scm_key);
  return paper_book;
}
