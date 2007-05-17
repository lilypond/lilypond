/*
  book.cc -- implement Book

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "book.hh"

#include <cstdio>
using namespace std;

#include "main.hh"
#include "music.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "score.hh"
#include "text-interface.hh"
#include "warn.hh"
#include "performance.hh"
#include "paper-score.hh"
#include "page-marker.hh"

#include "ly-smobs.icc"

Book::Book ()
{
  paper_ = 0;
  header_ = SCM_EOL;
  scores_ = SCM_EOL;
  input_location_ = SCM_EOL;
  smobify_self ();

  input_location_ = make_input (Input ());
}

Book::Book (Book const &s)
{
  paper_ = 0;
  header_ = SCM_EOL;
  scores_ = SCM_EOL;
  input_location_ = SCM_EOL;
  smobify_self ();

  if (s.paper_)
    {
      paper_ = s.paper_->clone ();
      paper_->unprotect ();
    }
  
  input_location_ = make_input (*s.origin ());

  header_ = ly_make_anonymous_module (false);
  if (ly_is_module (s.header_))
    ly_module_copy (header_, s.header_);
  
  SCM *t = &scores_;
  for (SCM p = s.scores_; scm_is_pair (p); p = scm_cdr (p))
    {
      Score *newscore = unsmob_score (scm_car (p))->clone ();

      *t = scm_cons (newscore->self_scm (), SCM_EOL);
      t = SCM_CDRLOC (*t);
      newscore->unprotect ();
    }
}

Input *
Book::origin () const
{
  return unsmob_input (input_location_);
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

  if (book->paper_)
    scm_gc_mark (book->paper_->self_scm ());
  scm_gc_mark (book->scores_);
  scm_gc_mark (book->input_location_);
  
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
  for (SCM s = scores_; scm_is_pair (s); s = scm_cdr (s))
    if (Score *score = unsmob_score (scm_car (s)))
      if (score->error_found_)
	return 0;

  Output_def *paper = paper_ ? paper_ : default_paper;
  if (!paper)
    return 0;
  
  Paper_book *paper_book = new Paper_book ();
  Real scale = scm_to_double (paper->c_variable ("output-scale"));
  Output_def *scaled_bookdef = scale_output_def (paper, scale);

  paper_book->paper_ = scaled_bookdef;
  scaled_bookdef->unprotect ();

  paper_book->header_ = header_;

  /* Render in order of parsing.  */
  for (SCM s = scm_reverse (scores_); scm_is_pair (s); s = scm_cdr (s))
    {
      if (Score *score = unsmob_score (scm_car (s)))
	{
	  SCM outputs = score
	    ->book_rendering (paper_book->paper_, default_layout);

	  while (scm_is_pair (outputs))
	    {
	      Music_output *output = unsmob_music_output (scm_car (outputs));

	      if (Performance *perf = dynamic_cast<Performance *> (output))
		paper_book->add_performance (perf->self_scm ());
	      else if (Paper_score *pscore = dynamic_cast<Paper_score *> (output))
		{
		  if (ly_is_module (score->header_))
		    paper_book->add_score (score->header_);
		  paper_book->add_score (pscore->self_scm ());
		}

	      outputs = scm_cdr (outputs);
	    }
	}
      else if (Text_interface::is_markup (scm_car (s))
	       || unsmob_page_marker (scm_car (s)))
	paper_book->add_score (scm_car (s));
      else
	assert (0);
    }

  return paper_book;
}

