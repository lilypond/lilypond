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
#include "warn.hh"

#include "ly-smobs.icc"

Book::Book ()
  : Input ()
{
  paper_ = 0;
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

#if 0
  if (book->key_)
    scm_gc_mark (book->key_->self_scm());
#endif
  if (book->paper_)
    scm_gc_mark (book->paper_->self_scm ());
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
  Real scale = scm_to_double (paper_->c_variable ("outputscale"));
  Output_def * scaled_bookdef = scale_output_def (paper_, scale);


  Object_key * key = new Lilypond_general_key (0, user_key_, 0);
  SCM  scm_key = key->self_scm();
  scm_gc_unprotect_object (scm_key);
  
  paper_book->paper_ = scaled_bookdef;
  scm_gc_unprotect_object (scaled_bookdef->self_scm());
  
  paper_book->header_ = header_;
  int score_count = scores_.size ();
  for (int i = 0; i < score_count; i++)
    {
      SCM systems = scores_[i]->book_rendering (outname,
						paper_book->paper_,
						default_def,
						key
						);
      
      /*
	If the score is empty, generate no output.  Should we
	do titling?
      */
      if (SCM_NFALSEP(scm_vector_p (systems)))
	{
	  Score_systems sc;
	  sc.systems_ = systems;
	  sc.header_ = scores_[i]->header_;
	  paper_book->score_systems_.push (sc);
	}
    }


  scm_remember_upto_here_1 (scm_key);
  return paper_book;
}


  

void
Book::add_score (Score *s)
{
  scores_.push (s);
  scm_gc_unprotect_object (s->self_scm ());
}
