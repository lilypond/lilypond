/*
  paper-book.cc -- implement Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

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
  Real hsize_;
  Real vsize_;

  Page (Paper_def*);
  void output (Paper_outputter*, bool);
};

Page::Page (Paper_def *paper)
{
  paper_ = paper;
  hsize_ = paper->get_realvar (ly_symbol2scm ("hsize"));
  vsize_ = paper->get_realvar (ly_symbol2scm ("vsize"));
}

void
Page::output (Paper_outputter *out, bool is_last)
{
  int line_count = SCM_VECTOR_LENGTH ((SCM) lines_);
  for (int i = 0; i < line_count; i++)
    out->output_line (scm_vector_ref (lines_, scm_int2num (i)),
		      is_last && i == line_count - 1);
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
  TODO:
     * get dimensions
     * title: bookTitle vs scoreTitle
     * start-page / stop-page in output-* interface
     * some page breaking algorithm
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
  for (int i = 0; i < score_count; i++)
    {
      Page *page = new Page (paper);
      page->lines_ = paper_scores_[i]->lines_;
      pages->push (page);
    }

  //if (paper_scores_[0]->book_title_)
  //  title = paper_scores_[0]->book_title_->extent (Y_AXIS).length ();

  return pages;
}
