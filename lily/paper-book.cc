/*
  paper-book.cc -- implement Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <stdio.h>

#include "main.hh"
#include "paper-book.hh"
#include "paper-score.hh"
#include "paper-def.hh"
#include "stencil.hh"
#include "paper-outputter.hh"

// WIP -- simplistic page interface
// Do we need this at all?  SCM, smob?
class Page
{
public:
  Paper_def *paper_;

  SCM lines_;
  Stencil *header_;
  Stencil *footer_;

  /* actual height filled with text.  */
  Real height_;
  
  //HMMM all this size stuff to paper/paper-outputter?
  Real hsize_;
  Real vsize_;
  Real foot_sep_;
  Real head_sep_;

  /* available area for text.  */
  Real text_height ();

  Page (Paper_def*);
  void output (Paper_outputter*, bool);
};

Page::Page (Paper_def *paper)
{
  paper_ = paper;
  height_ = 0;
  header_ = 0;
  footer_ = 0;
  lines_ = SCM_EOL;
  
  hsize_ = paper->get_realvar (ly_symbol2scm ("hsize"));
  vsize_ = paper->get_realvar (ly_symbol2scm ("vsize"));
  head_sep_ = 0; //paper->get_realvar (ly_symbol2scm ("head-sep"));
  foot_sep_ = 0; //paper->get_realvar (ly_symbol2scm ("foot-sep"));
}

void
Page::output (Paper_outputter *out, bool is_last)
{
#if ONE_SCORE_PER_PAGE
  int line_count = SCM_VECTOR_LENGTH ((SCM) lines_);
  for (int i = 0; i < line_count; i++)
    out->output_line (scm_vector_ref (lines_, scm_int2num (i)),
		      is_last && i == line_count - 1);
#else
  // TODO: header/footer etc
  out->output_scheme (scm_list_1 (ly_symbol2scm ("start-page")));
  for (SCM s = lines_; gh_pair_p (s); s = ly_cdr (s))
    out->output_line (ly_car (s), is_last && gh_pair_p (ly_cdr (s)));
  out->output_scheme (scm_list_2 (ly_symbol2scm ("stop-page"),
				  gh_bool2scm (is_last)));
#endif
}

Real
Page::text_height ()
{
  Real h = vsize_;
  if (header_)
    h -= header_->extent (Y_AXIS).length () + head_sep_;
  if (footer_)
    h -= footer_->extent (Y_AXIS).length () + foot_sep_;
  return h;
}


Paper_book *paper_book;

Paper_book::Paper_book ()
{
}

void
Paper_book::output ()
{
  Paper_outputter *out = paper_scores_[0]->outputter_;
  
  Paper_def *paper = paper_scores_[0]->paper_;
  out->output_header (paper);
  
  if (paper_scores_[0]->book_title_)
    out->output_expr (paper_scores_[0]->book_title_->get_expr (),
		      Offset (0, 0));

  Link_array<Page> *pages = get_pages ();
  int page_count = pages->size ();
  for (int i = 0; i < page_count; i++)
    (*pages)[i]->output (out, i + 1 == page_count);

  out->output_scheme (scm_list_1 (ly_symbol2scm ("end-output")));
  progress_indication ("\n");
}

/*
  WIP -- this simply adds one Page per \score

  FIXME: titling is broken.
  
  TODO:
     * ->SCM?
     * decent page breaking algorithm
     * header / footer (generate per Page, with page#)
     * override: # pages, or pageBreakLines= #'(3 3 4), ?
     * what about between-system-breaking, can we junk that?
     
*/
Link_array<Page>*
Paper_book::get_pages ()
{
  Link_array<Page> *pages = new Link_array<Page>;
  Paper_def *paper = paper_scores_[0]->paper_;
  int score_count = paper_scores_.size ();

  /* Calculate the full book height.  Hmm, can't we cache system
     heights while making stencils?  */
  Real book_height = 0;
  for (int i = 0; i < score_count; i++)
    {
      Paper_score *pscore = paper_scores_[i];
      Stencil *title = (i == 0 ? pscore->book_title_ : pscore->score_title_);
      if (title)
	book_height += title->extent (Y_AXIS).length ();

      int line_count = SCM_VECTOR_LENGTH ((SCM) pscore->lines_);
      for (int j = 0; j < line_count; j++)
	{
	  SCM line = scm_vector_ref (pscore->lines_, scm_int2num (j));
	  book_height += ly_scm2offset (ly_car (line))[Y_AXIS];
	}
    }

  Page *page = new Page (paper);
  fprintf (stderr, "book_height: %f\n", book_height);
  fprintf (stderr, "vsize: %f\n", page->vsize_);

#if ONE_SCORE_PER_PAGE
  for (int i = 0; i < score_count; i++)
    {
      page->lines_ = paper_scores_[i]->lines_;
      pages->push (page);
    }
#else
  /* Simplistic page breaking.  */
  Real text_height = page->text_height ();
  for (int i = 0; i < score_count; i++)
    {
      Paper_score *pscore = paper_scores_[i];
      Stencil *title = (i == 0 ? pscore->book_title_ : pscore->score_title_);
      Real h = 0;
      if (title)
	h = title->extent (Y_AXIS).length ();

      int line_count = SCM_VECTOR_LENGTH ((SCM) pscore->lines_);
      for (int j = 0; j < line_count; j++)
	{
	  SCM line = scm_vector_ref (pscore->lines_, scm_int2num (j));
	  h += ly_scm2offset (ly_car (line))[Y_AXIS];
	  if (page->height_ + h > text_height)
	    {
	      pages->push (page);
	      page = new Page (paper);
	    }
	  if (page->height_ + h <= text_height || page->height_ == 0)
	    {
	      if (j == 0 && title)
		page->lines_
		  = ly_snoc (scm_cons (ly_offset2scm (Offset (0, 0)),
				       scm_list_1
				       (scm_cons
					(ly_offset2scm (Offset (0, 0)),
					 title->smobbed_copy ()))),
			     page->lines_);
	      page->lines_ = ly_snoc (line, page->lines_);
	      page->height_ += h;
	      h = 0;
	    }
	}
    }
#endif
  
  pages->push (page);
  return pages;
}
