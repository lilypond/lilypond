/*
  book.cc -- implement Book

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "book.hh"

#include <cstdio>
using namespace std;

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
#include "text-interface.hh"
#include "warn.hh"

#include "performance.hh"
#include "paper-score.hh"

#include "ly-smobs.icc"

Book::Book ()
  : Input ()
{
  paper_ = 0;
  header_ = SCM_EOL;
  scores_ = SCM_EOL;
  smobify_self ();
}

Book* 
Book::clone () const
{
  return new Book (*this);
}

Book::~Book ()
{
}

IMPLEMENT_SMOBS (Book);
IMPLEMENT_DEFAULT_EQUAL_P (Book);

SCM
Book::mark_smob (SCM s)
{
  Book *book = (Book *) SCM_CELL_WORD_1 (s);

#if 0
  if (book->key_)
    scm_gc_mark (book->key_->self_scm ());
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

/* Concatenate all score outputs into a Paper_book
 */
Paper_book *
Book::process (Output_def *default_paper,
	       Output_def *default_layout)
{
  for (SCM s = scores_; s != SCM_EOL; s = scm_cdr (s))
    if (Score *score = unsmob_score (scm_car (s)))
      if (score->error_found_)
	return 0;

  Output_def *paper = paper_ ? paper_ : default_paper;
  if (!paper)
    return 0;
  
  Paper_book *paper_book = new Paper_book ();
  Real scale = scm_to_double (paper->c_variable ("outputscale"));
  Output_def *scaled_bookdef = scale_output_def (paper, scale);

  Object_key *key = new Lilypond_general_key (0, user_key_, 0);
  SCM scm_key = key->unprotect ();

  paper_book->paper_ = scaled_bookdef;
  scaled_bookdef->unprotect ();

  paper_book->header_ = header_;

  /* Render in order of parsing.  */
  for (SCM s = scm_reverse (scores_); s != SCM_EOL; s = scm_cdr (s))
    {
      if (Score *score = unsmob_score (scm_car (s)))
	{
	  SCM outputs = score
	    ->book_rendering (paper_book->paper_, default_layout, key);

	  while (scm_is_pair (outputs))
	    {
	      Music_output *output = unsmob_music_output (scm_car (outputs));

	      if (Performance *perf = dynamic_cast<Performance *> (output))
		paper_book->add_performance (perf->self_scm ());
	      else if (Paper_score *pscore = dynamic_cast<Paper_score *> (output))
		{
		  SCM systems = pscore->get_paper_systems ();
		  if (ly_is_module (score->header_))
		    paper_book->add_score (score->header_);
		  paper_book->add_score (systems);
		}

	      outputs = scm_cdr (outputs);
	    }
	}
      else if (Text_interface::is_markup (scm_car (s)))
	paper_book->add_score (scm_car (s));
      else
	assert (0);
    }

  scm_remember_upto_here_1 (scm_key);
  return paper_book;
}

