/*
  page.cc -- implement Page

  source file of the GNU LilyPond music typesetter

  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "dimensions.hh"
#include "ly-module.hh"
#include "page.hh"
#include "output-def.hh"
#include "paper-outputter.hh"
#include "paper-line.hh"
#include "stencil.hh"
#include "warn.hh"


Real Page::MIN_COVERAGE_ = 0.66;

Page::Page (SCM lines, Output_def *paper, int number)
{
  copyright_ = SCM_EOL;
  footer_ = SCM_EOL;
  header_ = SCM_EOL;
  lines_ = SCM_EOL;
  tagline_ = SCM_EOL;

  smobify_self ();

  
  paper_ = paper;
  number_ = number;

  height_ = 0;
  line_count_ = 0;
  is_last_ = false;
  header_ = scm_call_2 (paper_->c_variable ("make-header"),
			paper_->self_scm (),
			scm_int2num (number_));
  if (unsmob_stencil (header_))
    unsmob_stencil (header_)->align_to (Y_AXIS, UP);
    
  footer_ = scm_call_2 (paper_->c_variable ("make-footer"),
			paper_->self_scm (),
			scm_int2num (number_));
  if (unsmob_stencil (footer_))
    unsmob_stencil (footer_)->align_to (Y_AXIS, UP);

  lines_ = lines;
  for (SCM s = lines; ly_c_pair_p (s); s = ly_cdr (s))
    {
      height_ += unsmob_paper_line (ly_car (s))->dim()[Y_AXIS];
      line_count_ ++;
    }
  
}

Page::~Page ()
{
}

#include "ly-smobs.icc"

IMPLEMENT_DEFAULT_EQUAL_P (Page)
IMPLEMENT_SMOBS (Page)
IMPLEMENT_TYPE_P (Page, "ly:page?")

SCM
Page::mark_smob (SCM smob)
{
  Page *p = (Page*) SCM_CELL_WORD_1 (smob);
  scm_gc_mark (p->header_);
  scm_gc_mark (p->footer_);

  if (p->paper_)
    {
      scm_gc_mark (p->paper_->self_scm ());
    }
  
  scm_gc_mark (p->copyright_);
  scm_gc_mark (p->tagline_);
  //scm_gc_mark (p->lines_);
  return p->lines_;
}

int
Page::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Page *p = (Page*) ly_cdr (smob);
  scm_puts ("#<", port);
  scm_puts (classname (p), port);
  scm_puts (to_string (p->number_).to_str0 (), port);
  scm_puts (" ", port);
  scm_puts (">", port);
  return 1;
}

static Stencil
stack_stencils (Stencil a, Stencil b, Offset *origin)
{
  Real height = b.extent (Y_AXIS).length ();
  if (height > 50 CM)
    {
      programming_error (to_string ("Improbable stencil height: %f", height));
      height = 50 CM;
    }
  Offset o = *origin;
  o.mirror (Y_AXIS);
  b.translate (o);
  a.add_stencil (b);
  (*origin)[Y_AXIS] += height;
  return a;
}

Stencil
Page::to_stencil () const
{
  SCM proc = paper_->lookup_variable (ly_symbol2scm ("page-to-stencil"));
  return *unsmob_stencil (scm_call_1 (proc, self_scm ()));
}

//urg
Real
Page::left_margin () const
{
  return (paper_->get_dimension (ly_symbol2scm ("hsize"))
	  - paper_->get_dimension (ly_symbol2scm ("linewidth"))) / 2;
}

LY_DEFINE (ly_page_header_lines_footer_stencil, "ly:page-header-lines-footer-stencil",
	   1, 0, 0, (SCM page),
	   "Simple header, lines, footer stencil from PAGE.")
{
  Page *p = unsmob_page (page);
  SCM_ASSERT_TYPE (p, page, SCM_ARG1, __FUNCTION__, "page");
  
  Stencil stencil;
  Offset o (p->left_margin (),
	    p->paper_->get_dimension (ly_symbol2scm ("top-margin")));

  Real vfill = (p->line_count_ > 1
		? (p->text_height () - p->height_) / (p->line_count_ - 1)
		: 0);

  Real coverage = p->height_ / p->text_height ();
  if (coverage < p->MIN_COVERAGE_)
    /* Do not space out a badly filled page.  This is too simplistic
       (ie broken), because this should not vary too much between
       (subsequent?) pages in a book.  */
    vfill = 0;

  if (Stencil *s = unsmob_stencil (p->header_))
    {
      stencil = stack_stencils (stencil, *s, &o);
      o[Y_AXIS] += p->paper_->get_dimension (ly_symbol2scm ("head-sep"));
    }

  for (SCM s = p->lines_; s != SCM_EOL; s = ly_cdr (s))
    {
      Paper_line *p = unsmob_paper_line (ly_car (s));
      stencil = stack_stencils (stencil, p->to_stencil (), &o);
      /* Do not put vfill between title and its music, */
      if (ly_cdr (s) != SCM_EOL
	  && (!p->is_title () || vfill < 0))
	o[Y_AXIS] += vfill;
      /* rather put extra just before the title.  */
      if (ly_cdr (s) != SCM_EOL
	  && (unsmob_paper_line (ly_cadr (s))->is_title () && vfill > 0))
	o[Y_AXIS] += vfill;
    }

  o[Y_AXIS] = p->paper_->get_dimension (ly_symbol2scm ("vsize"))
    - p->paper_->get_dimension (ly_symbol2scm ("bottom-margin"));
  if (unsmob_stencil (p->copyright_))
    o[Y_AXIS] -= unsmob_stencil (p->copyright_)->extent (Y_AXIS).length ();
  if (unsmob_stencil (p->tagline_))
    o[Y_AXIS] -= unsmob_stencil (p->tagline_)->extent (Y_AXIS).length ();
  if (unsmob_stencil (p->footer_))
    o[Y_AXIS] -= unsmob_stencil (p->footer_)->extent (Y_AXIS).length ();

  if (Stencil *s = unsmob_stencil (p->copyright_))
    stencil = stack_stencils (stencil, *s, &o);
  if (Stencil *s = unsmob_stencil (p->tagline_))
    stencil = stack_stencils (stencil, *s, &o);
  if (Stencil *s = unsmob_stencil (p->footer_))
    stencil = stack_stencils (stencil, *s, &o);

  return stencil.smobbed_copy ();
}

Real
Page::text_height () const
{
  Real h = paper_->get_dimension (ly_symbol2scm ("vsize"))
    - paper_->get_dimension (ly_symbol2scm ("top-margin"))
    - paper_->get_dimension (ly_symbol2scm ("bottom-margin"));
  if (unsmob_stencil (header_))
    h -= unsmob_stencil (header_)->extent (Y_AXIS).length ()
      + paper_->get_dimension (ly_symbol2scm ("head-sep"));
  if (unsmob_stencil (copyright_)
      || unsmob_stencil (tagline_)
      || unsmob_stencil (footer_))
    h -= paper_->get_dimension (ly_symbol2scm ("foot-sep"));
  if (unsmob_stencil (copyright_))
    h -= unsmob_stencil (copyright_)->extent (Y_AXIS).length ();
  if (unsmob_stencil (tagline_))
    h -= unsmob_stencil (tagline_)->extent (Y_AXIS).length ();
  if (unsmob_stencil (footer_))
    h -= unsmob_stencil (footer_)->extent (Y_AXIS).length ();
  return h;
}


/*
  TODO: unused?
  
 */
LY_DEFINE (ly_page_paper_lines, "ly:page-paper-lines",
	   1, 0, 0, (SCM page),
	   "Return paper-lines from @var{page}.")
{
  Page *p = unsmob_page (page);
  SCM_ASSERT_TYPE (p, page, SCM_ARG1, __FUNCTION__, "page");
  return p->lines_;
}

LY_DEFINE (ly_page_stencil, "ly:page-stencil",
	   1, 0, 0, (SCM page),
	   "Return stencil for @var{page}.")
{
  Page *p = unsmob_page (page);
  SCM_ASSERT_TYPE (p, page, SCM_ARG1, __FUNCTION__, "page");
  return p->to_stencil ().smobbed_copy ();
}


LY_DEFINE (ly_page_last_p, "ly:page-last?",
	   1, 0, 0, (SCM page),
	   "Is @var{page} the last one?")
{
  Page *p = unsmob_page (page);
  SCM_ASSERT_TYPE (p, page, SCM_ARG1, __FUNCTION__, "page");
  return ly_bool2scm (p->is_last_);
}
