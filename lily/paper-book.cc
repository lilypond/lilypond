/*
  paper-book.cc -- implement Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <stdio.h>
#include <math.h>

#include "ly-module.hh"
#include "main.hh"
#include "paper-book.hh"
#include "paper-def.hh"
#include "paper-outputter.hh"
#include "paper-score.hh"
#include "stencil.hh"

static Real const MIN_COVERAGE = 0.66;
static Real const MAX_CRAMP = 0.05;

static SCM
stencil2line (Stencil* stil)
{
  static SCM z;
  if (!z)
    z = scm_permanent_object (ly_offset2scm (Offset (0, 0)));
  Offset dim = Offset (stil->extent (X_AXIS).length (),
		       stil->extent (Y_AXIS).length ());
  return scm_cons (ly_offset2scm (dim),
		   scm_list_1 (scm_cons (z, stil->smobbed_copy ())));
}

static SCM
title2line (Stencil* stil)
{
  SCM s = stencil2line (stil);
  /* whugh, add marker recognise titles.  */
  return scm_cons (scm_cons (ly_symbol2scm ("title"), ly_car (s)), ly_cdr (s));
}

/* Simplistic page interface */
class Page
{
public:
  Paper_def *paper_;
  static int page_count_;
  int number_;
  int line_count_;

  Protected_scm lines_;
  Protected_scm header_;
  Protected_scm footer_;
  Protected_scm copyright_;
  Protected_scm tagline_;
  
  Stencil *get_header () { return unsmob_stencil (header_); }
  Stencil *get_copyright () { return unsmob_stencil (copyright_); }
  Stencil *get_tagline () { return unsmob_stencil (tagline_); }
  Stencil *get_footer () { return unsmob_stencil (footer_); }

  /* actual height filled with text.  */
  Real height_;
  
  // HMMM all this size stuff to paper/paper-outputter?
  Real hsize_;
  Real vsize_;
  Real left_margin_;
  Real top_margin_;
  Real bottom_margin_;
  Real foot_sep_;
  Real head_sep_;
  Real text_width_;

  /* available area for text.  */
  Real text_height ();

  Page (Paper_def*, int);
  void output (Paper_outputter*, bool);
};

int Page::page_count_ = 0;

Page::Page (Paper_def *paper, int number)
{
  paper_ = paper;
  number_ = number;
  page_count_++;
  
  height_ = 0;
  lines_ = SCM_EOL;
  line_count_ = 0;

  hsize_ = paper->get_realvar (ly_symbol2scm ("hsize"));
  vsize_ = paper->get_realvar (ly_symbol2scm ("vsize"));
  top_margin_ = paper->get_realvar (ly_symbol2scm ("top-margin"));
  bottom_margin_ = paper->get_realvar (ly_symbol2scm ("bottom-margin"));
  head_sep_ = paper->get_realvar (ly_symbol2scm ("head-sep"));
  foot_sep_ = paper->get_realvar (ly_symbol2scm ("foot-sep"));
  text_width_ = paper->get_realvar (ly_symbol2scm ("linewidth"));
  left_margin_ = (hsize_ - text_width_) / 2;
  
  copyright_ = SCM_EOL;
  tagline_ = SCM_EOL;
  
  SCM make_header = scm_primitive_eval (ly_symbol2scm ("make-header"));
  SCM make_footer = scm_primitive_eval (ly_symbol2scm ("make-footer"));

  header_ = scm_call_2 (make_header, paper_->smobbed_copy (),
			scm_int2num (number_));
  // FIXME: why does this (generates Stencil) not trigger font load?
  if (get_header ())
    get_header ()->align_to (Y_AXIS, UP);
    
  footer_ = scm_call_2 (make_footer, paper_->smobbed_copy (),
			scm_int2num (number_));
  if (get_footer ())
    get_footer ()->align_to (Y_AXIS, UP);
}

void
Page::output (Paper_outputter *out, bool is_last)
{
  progress_indication ("[" + to_string (number_));
  out->output_scheme (scm_list_1 (ly_symbol2scm ("start-page")));
  Offset o (left_margin_, top_margin_);
  Real vfill = line_count_ > 1 ? (text_height () - height_) / (line_count_ - 1)
    : 0;

  Real coverage = height_ / text_height ();
  if (coverage < MIN_COVERAGE)
    /* Do not space out a badly filled page.  This is too simplistic
       (ie broken), because this should not vary too much between
       (subsequent?) pages in a book.  */
    vfill = 0;

  if (get_header ())
    {
      out->output_line (stencil2line (get_header ()), &o, false);
      o[Y_AXIS] += head_sep_;
    }
  for (SCM s = lines_; gh_pair_p (s); s = ly_cdr (s))
    {
      SCM line = ly_car (s);
      SCM offset = ly_car (line);
      if (ly_car (offset) == ly_symbol2scm ("title"))
	line = scm_cons (ly_cdr (offset), ly_cdr (line));
      out->output_line (line, &o,
			is_last && gh_pair_p (ly_cdr (s)) && !get_copyright ()
			&& !get_tagline () && !get_footer ());
      if (gh_pair_p (ly_cdr (s)) && ly_car (offset) != ly_symbol2scm ("title"))
	o[Y_AXIS] += vfill; 
    }

#if 0
  if (get_copyright () || get_tagline () || get_footer ())
    o[Y_AXIS] += foot_sep_;
#else
  o[Y_AXIS] = vsize_ - bottom_margin_;
  if (get_copyright ())
    o[Y_AXIS] -= get_copyright ()->extent (Y_AXIS).length ();
  if (get_tagline ())
    o[Y_AXIS] -= get_tagline ()->extent (Y_AXIS).length ();
  if (get_footer ())
    o[Y_AXIS] -= get_footer ()->extent (Y_AXIS).length ();
#endif

  if (get_copyright ())
    out->output_line (stencil2line (get_copyright ()), &o,
		      is_last && !get_tagline () && !get_footer ());
  if (get_tagline ())
    out->output_line (stencil2line (get_tagline ()), &o,
		      is_last && !get_footer ());
  if (get_footer ())
    out->output_line (stencil2line (get_footer ()), &o, is_last);
  out->output_scheme (scm_list_2 (ly_symbol2scm ("stop-page"),
				  gh_bool2scm (is_last && !get_footer ())));
  progress_indication ("]");
}

Real
Page::text_height ()
{
  Real h = vsize_ - top_margin_ - bottom_margin_;
  if (get_header ())
    h -= get_header ()->extent (Y_AXIS).length () + head_sep_;
  if (get_copyright () || get_tagline () || get_footer ())
    h -= foot_sep_;
  if (get_copyright ())
    h -= get_copyright ()->extent (Y_AXIS).length ();
  if (get_tagline ())
    h -= get_tagline ()->extent (Y_AXIS).length ();
  if (get_footer ())
    h -= get_footer ()->extent (Y_AXIS).length ();
  return h;
}

/****************************************************************/

/* Current global paper book.  Gives default_rendering access from
   input-file-results.  */
Paper_book *paper_book;

Paper_book::Paper_book ()
{
  copyright_ = SCM_EOL;
  tagline_ = SCM_EOL;
  
  smobify_self ();
}

Paper_book::~Paper_book ()
{
}

void
Paper_book::output (String outname)
{
  if (!papers_.size ())
    return;
    
  /* Generate all stencils to trigger font loads.  */
  Link_array<Page> *pages = get_pages ();

  Paper_def *paper = papers_[0];
  Paper_outputter *out = paper->get_paper_outputter (outname);
  out->output_header (paper, get_scopes (0), pages->size ());

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
  SCM user_title = scm_primitive_eval (ly_symbol2scm ("user-title"));
  SCM book_title = scm_primitive_eval (ly_symbol2scm ("book-title"));
  SCM score_title = scm_primitive_eval (ly_symbol2scm ("score-title"));
  SCM field = (i == 0 ? ly_symbol2scm ("bookTitle")
	       : ly_symbol2scm ("scoreTitle"));

  Stencil *title = 0;
  SCM scopes = get_scopes (i);
  SCM s = ly_modules_lookup (scopes, field);
  if (s != SCM_UNDEFINED && scm_variable_bound_p (s) == SCM_BOOL_T)
    title = unsmob_stencil (scm_call_2 (user_title,
					papers_[0]->self_scm (),
					scm_variable_ref (s)));
  else
    title = unsmob_stencil (scm_call_2 (i == 0 ? book_title : score_title,
					papers_[0]->self_scm (),
					scopes));
  if (title)
    title->align_to (Y_AXIS, UP);
  
  return title;
}

/* calculate book height, #lines, stencils.  */
void
Paper_book::init ()
{
  int score_count = scores_.size ();

  /* Calculate the full book height.  Hmm, can't we cache system
     heights while making stencils?  */
  height_ = 0;
  for (int i = 0; i < score_count; i++)
    {
      Stencil *title = get_title (i);
      if (title)
	height_ += title->extent (Y_AXIS).length ();

      int line_count = SCM_VECTOR_LENGTH ((SCM) scores_[i]);
      for (int j = 0; j < line_count; j++)
	{
	  SCM line = scm_vector_ref ((SCM) scores_[i], scm_int2num (j));
	  height_ += ly_scm2offset (ly_car (line))[Y_AXIS];
	}
    }

  Paper_def *paper = papers_[0];
  SCM scopes = get_scopes (0);

  SCM make_tagline = scm_primitive_eval (ly_symbol2scm ("make-tagline"));
  tagline_ = scm_call_2 (make_tagline, paper->smobbed_copy (), scopes);
  Real tag_height = 0;
  if (Stencil *s = unsmob_stencil (tagline_))
    tag_height = s->extent (Y_AXIS).length ();
  height_ += tag_height;

  SCM make_copyright = scm_primitive_eval (ly_symbol2scm ("make-copyright"));
  copyright_ = scm_call_2 (make_copyright, paper->smobbed_copy (), scopes);
  Real copy_height = 0;
  if (Stencil *s = unsmob_stencil (copyright_))
    copy_height = s->extent (Y_AXIS).length ();
  height_ += copy_height;
}

/* Ideas:
   - real page breaking algorithm (Gourlay?)
     Hmmughr, Gourlay uses Grobs, columns etc -- looks like it needs serious
     refactoring before it can be applied to Page breaking...
   - override: # pages, or pageBreakLines= #'(3 3 4), ?  */
Link_array<Page>*
Paper_book::get_pages ()
{
  init ();
  Page::page_count_ = 0;
  Paper_def *paper = papers_[0];
  Page *page = new Page (paper, 1);

  Real text_height = page->text_height ();
  Real page_frac = height_ / text_height;
  int page_count = (int) ceil (page_frac);
  if (unsmob_stencil (copyright_))
    page->copyright_ = copyright_;
  if (unsmob_stencil (tagline_) && page_count == 1)
    page->tagline_ = tagline_;

  /* Attempt to fill pages better using FUDGE kludge.  */
  Real r = page_frac - (int) page_frac;
  Real cramp_fudge = page_count > 1 ? (r / (page_count - 1)) * text_height : 0;
  Real expand_fudge = - ((1 - r) / page_count) * text_height;
  
  Link_array<Page>* pages = 0;
  if ((page_count > 1 && (r / (page_count - 1)) < MAX_CRAMP))
    {
      /* Just a little more space (<MAX_CRAMP) per page is needed,
	 cramp onto one page less.  */
      pages = fill_pages (page, page_count - 1, cramp_fudge);
      if (pages->size () != page_count - 1)
	{
	  /* Cramping failed.  */
	  page = pages->get (0);
	  delete pages;
	  pages = 0;
	}
    }      
  if (!pages && ((1 - r) / page_count) < 1 - MIN_COVERAGE)
    {
      /* There is space left, but not so much that paged would have too
	 little blackness (< MIN_COVERAGE), distribute evenly.  */
      pages = fill_pages (page, page_count, expand_fudge);
      bool badly_covered = false;
      if (pages->size () == page_count)
	for (int i = 0; i < page_count; i++)
	  {
	    Page *p = (*pages)[i];
	    Real coverage = p ->height_ / p->text_height ();
	    if (coverage < MIN_COVERAGE)
	      {
		badly_covered = true;
		break;
	      }
	  }
      if (pages->size () != page_count || badly_covered)
	{
	  /* expanding failed.  */
	  page = pages->get (0);
	  delete pages;
	  pages = 0;
	}
    }
  
  if (!pages)
    /* Fudging failed; just fill pages.  */
    pages = fill_pages (page, page_count, 0);
  return pages;
}

/* Simplistic page breaking:
   add lines until HEIGHT > PAGE.TEXT_HEIGHT_ + FUDGE  */
Link_array<Page>*
Paper_book::fill_pages (Page *page, int page_count, Real fudge)
{
  int page_number = 1;
  int score_count = scores_.size ();
  Paper_def *paper = papers_[0];
  Link_array<Page> *pages = new Link_array<Page>;
  page->lines_ = SCM_EOL;
  page->height_ = 0;
  page->line_count_ = 0;
  Real text_height = page->text_height ();
  for (int i = 0; i < score_count; i++)
    {
      Real h = 0;
      Stencil *title = get_title (i);
      if (title)
	h = title->extent (Y_AXIS).length ();

      int line_count = SCM_VECTOR_LENGTH ((SCM) scores_[i]);
      for (int j = 0; j < line_count; j++)
	{
	  SCM line = scm_vector_ref ((SCM) scores_[i], scm_int2num (j));
	  h += ly_scm2offset (ly_car (line))[Y_AXIS];
	  Real fill = (page->height_ - text_height) / text_height;
	  // Real fill_h = (page->height_ + h - text_height) / text_height;
	  Real fudged_fill = (page->height_ - (text_height + fudge))
	    / (text_height + fudge);
	  Real fudged_fill_h = ((page->height_ + h) - (text_height + fudge))
	    / (text_height + fudge);
	  if (fill > -MAX_CRAMP
	      || (fudged_fill > -MAX_CRAMP
		  && (fudge < 0
		      || !(fudged_fill_h > 0
			   && abs (fudged_fill_h) < 4 * abs (fudged_fill)))))
	    {
	      pages->push (page);
	      page = new Page (paper, ++page_number);
	      if (unsmob_stencil (tagline_) && page_number == page_count)
		page->tagline_ = tagline_;
	      text_height = page->text_height ();
	    }
	  if (j == 0 && title)
	    page->lines_ = ly_snoc (title2line (title), page->lines_);
	  page->lines_ = ly_snoc (line, page->lines_);
	  page->line_count_++;
	  page->height_ += h;
	  h = 0;
	}
    }

  pages->push (page);
  return pages;
}

void
Paper_book::classic_output (String outname)
{
  int count = scores_.size ();
  Paper_outputter *out = papers_.top ()->get_paper_outputter (outname);
  out->output_header (papers_.top (), get_scopes (count - 1), 0);

  int line_count = SCM_VECTOR_LENGTH ((SCM) scores_.top ());
  for (int i = 0; i < line_count; i++)
    out->output_line (scm_vector_ref ((SCM) scores_.top (), scm_int2num (i)),
		      0, i == line_count - 1);
  
  out->output_scheme (scm_list_1 (ly_symbol2scm ("end-output")));
  progress_indication ("\n");
}


#include "ly-smobs.icc"

IMPLEMENT_DEFAULT_EQUAL_P (Paper_book);
IMPLEMENT_SMOBS (Paper_book)
IMPLEMENT_TYPE_P (Paper_book, "ly:paper-book?")

SCM
Paper_book::mark_smob (SCM smob)
{
  Paper_book *pb = (Paper_book*) SCM_CELL_WORD_1 (smob);
  for (int i = 0; i < pb->headers_.size (); i++)
    scm_gc_mark (pb->headers_[i]);
  for (int i = 0; i < pb->global_headers_.size (); i++)
    scm_gc_mark (pb->global_headers_[i]);
  for (int i = 0; i < pb->papers_.size (); i++)
    scm_gc_mark (pb->papers_[i]->self_scm ());
  for (int i = 0; i < pb->scores_.size (); i++)
    scm_gc_mark (pb->scores_[i]);

  scm_gc_mark (pb->copyright_);

  
  return pb->tagline_;
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
