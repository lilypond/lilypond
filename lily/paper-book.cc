/*
  paper-book.cc -- implement Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <stdio.h>

#include "ly-module.hh"
#include "main.hh"
#include "paper-book.hh"
#include "paper-def.hh"
#include "paper-outputter.hh"
#include "paper-score.hh"
#include "stencil.hh"

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
  // TODO: header/footer etc
  out->output_scheme (scm_list_1 (ly_symbol2scm ("start-page")));
  for (SCM s = lines_; gh_pair_p (s); s = ly_cdr (s))
    out->output_line (ly_car (s), is_last && gh_pair_p (ly_cdr (s)));
  out->output_scheme (scm_list_2 (ly_symbol2scm ("stop-page"),
				  gh_bool2scm (is_last)));
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
  protect_ = SCM_EOL;
}

void
Paper_book::output (String outname)
{
  Paper_outputter *out = papers_.top ()->get_paper_outputter (outname);

  out->output_metadata (get_scopes (0), papers_.top ());
  out->output_header (papers_.top ());

  Link_array<Page> *pages = get_pages ();
  int page_count = pages->size ();
  for (int i = 0; i < page_count; i++)
    (*pages)[i]->output (out, i + 1 == page_count);

  out->output_scheme (scm_list_1 (ly_symbol2scm ("end-output")));
  progress_indication ("\n");
}

SCM
Paper_book::get_scopes (int i)
{
  SCM scopes = SCM_EOL;
  if (headers_[i])
    scopes = scm_cons (headers_[i], scopes);
  if (global_headers_[i] && global_headers_[i] != headers_[i])
    scopes = scm_cons (global_headers_[i], scopes);
  return scopes;
}

Stencil*
Paper_book::get_title (int i)
{
  SCM make_title = scm_primitive_eval (ly_symbol2scm ("make-title"));
  SCM field = (i == 0 ? ly_symbol2scm ("bookTitle")
	       : ly_symbol2scm ("scoreTitle"));

  SCM s = ly_modules_lookup (get_scopes (i), field); 
  if (s != SCM_UNDEFINED && scm_variable_bound_p (s) == SCM_BOOL_T)
    return unsmob_stencil (gh_call2 (make_title,
				     papers_[i]->self_scm (),
				     scm_variable_ref (s)));
  return 0;
}

/*
  WIP

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
  int score_count = scores_.size ();

  /* Calculate the full book height.  Hmm, can't we cache system
     heights while making stencils?  */
  Real book_height = 0;
  for (int i = 0; i < score_count; i++)
    {
      //SCM lines = scores_[i];
      Stencil *title = get_title (i);
      if (title)
	book_height += title->extent (Y_AXIS).length ();

      int line_count = SCM_VECTOR_LENGTH ((SCM) scores_[i]);
      for (int j = 0; j < line_count; j++)
	{
	  SCM line = scm_vector_ref ((SCM) scores_[i], scm_int2num (j));
	  book_height += ly_scm2offset (ly_car (line))[Y_AXIS];
	}
    }

  Page *page = new Page (papers_.top ());
  fprintf (stderr, "book_height: %f\n", book_height);
  fprintf (stderr, "vsize: %f\n", page->vsize_);
  fprintf (stderr, "pages: %f\n", book_height / page->text_height ());

  /* Simplistic page breaking.  */
  Real text_height = page->text_height ();
  for (int i = 0; i < score_count; i++)
    {
      Stencil *title = get_title (i);
      if (title)
	book_height += title->extent (Y_AXIS).length ();
      Real h = 0;
      if (title)
	h = title->extent (Y_AXIS).length ();

      int line_count = SCM_VECTOR_LENGTH ((SCM) scores_[i]);
      for (int j = 0; j < line_count; j++)
	{
	  SCM line = scm_vector_ref ((SCM) scores_[i], scm_int2num (j));
	  h += ly_scm2offset (ly_car (line))[Y_AXIS];
	  if (page->height_ + h > text_height)
	    {
	      pages->push (page);
	      page = new Page (papers_.top ());
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
  
  pages->push (page);
  return pages;
}

void
Paper_book::classic_output (String outname)
{
  Paper_outputter *out = papers_.top ()->get_paper_outputter (outname);
  int count = scores_.size ();
  
  out->output_metadata (get_scopes (count - 1), papers_.top ());
  out->output_header (papers_.top ());

  int line_count = SCM_VECTOR_LENGTH ((SCM) scores_.top ());
  for (int i = 0; i < line_count; i++)
    out->output_line (scm_vector_ref ((SCM) scores_.top (), scm_int2num (i)),
		      i == line_count - 1);
  
  out->output_scheme (scm_list_1 (ly_symbol2scm ("end-output")));
  progress_indication ("\n");
}


#include "ly-smobs.icc"

IMPLEMENT_DEFAULT_EQUAL_P (Paper_book);
IMPLEMENT_SIMPLE_SMOBS (Paper_book)
IMPLEMENT_TYPE_P (Paper_book, "ly:paper_book?")

SCM
Paper_book::mark_smob (SCM)
{
  return SCM_EOL;
}

int
Paper_book::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Paper_book *b = (Paper_book*) ly_cdr (smob);
     
  scm_puts ("#<", port);
  scm_puts (classname (b), port);
  scm_puts (" ", port);
  //scm_puts (b->, port);
  scm_puts (">", port);
  return 1;
}

SCM
Paper_book::smobbed_copy () const
{
  Paper_book *b = new Paper_book (*this);
  return b->smobbed_self ();
}
