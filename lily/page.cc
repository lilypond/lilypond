/*
  page.cc -- implement Page

  source file of the GNU LilyPond music typesetter

  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "dimensions.hh"
#include "ly-module.hh"
#include "page.hh"
#include "paper-def.hh"
#include "paper-outputter.hh"
#include "paper-line.hh"
#include "stencil.hh"
#include "warn.hh"

int Page::page_count_ = 0;
Real Page::MIN_COVERAGE_ = 0.66;

Page::Page (Paper_def *paper, int number)
{
  paper_ = paper;
  number_ = number;

  copyright_ = SCM_EOL;
  footer_ = SCM_EOL;
  header_ = SCM_EOL;
  lines_ = SCM_EOL;
  tagline_ = SCM_EOL;
  
  height_ = 0;
  line_count_ = 0;
  
  page_count_++;

  hsize_ = paper->get_dimension (ly_symbol2scm ("hsize"));
  vsize_ = paper->get_dimension (ly_symbol2scm ("vsize"));
  top_margin_ = paper->get_dimension (ly_symbol2scm ("top-margin"));
  bottom_margin_ = paper->get_dimension (ly_symbol2scm ("bottom-margin"));
  head_sep_ = paper->get_dimension (ly_symbol2scm ("head-sep"));
  foot_sep_ = paper->get_dimension (ly_symbol2scm ("foot-sep"));
  text_width_ = paper->get_dimension (ly_symbol2scm ("linewidth"));
  left_margin_ = (hsize_ - text_width_) / 2;
  
  SCM make_header = ly_scheme_function ("make-header");
  SCM make_footer = ly_scheme_function ("make-footer");

  header_ = scm_call_2 (make_header, paper_->self_scm (),
			scm_int2num (number_));
  if (unsmob_stencil (header_))
    unsmob_stencil (header_)->align_to (Y_AXIS, UP);
    
  footer_ = scm_call_2 (make_footer, paper_->self_scm (),
			scm_int2num (number_));
  if (unsmob_stencil (footer_))
    unsmob_stencil (footer_)->align_to (Y_AXIS, UP);

  smobify_self ();
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
  scm_gc_mark (p->lines_);
  scm_gc_mark (p->header_);
  scm_gc_mark (p->footer_);
  scm_gc_mark (p->copyright_);
  scm_gc_mark (p->tagline_);
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

static void
stack_stencils (Stencil &a, Stencil *b, Offset *origin)
{
  Real height = b->extent (Y_AXIS).length ();
  if (height > 50 CM)
    {
      programming_error (to_string ("Improbable stencil height: %f", height));
      height = 50 CM;
    }
  Offset o = *origin;
  o.mirror (Y_AXIS);
  b->translate (o);
  a.add_stencil (*b);
  (*origin)[Y_AXIS] += height;
}

SCM
Page::to_stencil () const
{
  SCM proc = ly_scheme_function ("page-to-stencil");
  return scm_call_1 (proc, self_scm ());
}

LY_DEFINE (ly_page_header_lines_footer_stencil, "ly:page-header-lines-footer-stencil",
	   1, 0, 0, (SCM page),
	   "Simple header, lines, footer stencil from PAGE.")
{
  Page *p = unsmob_page (page);
  SCM_ASSERT_TYPE (p, page, SCM_ARG1, __FUNCTION__, "page");
  
  Stencil stencil;
  Offset o (p->left_margin_, p->top_margin_);
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
      stack_stencils (stencil, s, &o);
      o[Y_AXIS] += p->head_sep_;
    }
  for (SCM s = p->lines_; s != SCM_EOL; s = ly_cdr (s))
    {
      Paper_line *p = unsmob_paper_line (ly_car (s));
#if 0 // ugh, parse error?
      Stencil line (Box (Interval (0, p->dim ()[X_AXIS]),
			 Interval (0, p->dim ()[Y_AXIS])), SCM_EOL);
#else
      Box x (Interval (0, p->dim ()[X_AXIS]),
	     Interval (0, p->dim ()[Y_AXIS]));
      Stencil line (x, SCM_EOL);
#endif
      line.add_stencil (*unsmob_stencil (p->to_stencil ()));
      stack_stencils (stencil, &line, &o);
      
      /* Do not put vfill between title and its music, */
      if (ly_cdr (s) != SCM_EOL
	  && (!p->is_title () || vfill < 0))
	o[Y_AXIS] += vfill;
      /* rather put extra just before the title.  */
      if (ly_cdr (s) != SCM_EOL
	  && (unsmob_paper_line (ly_cadr (s))->is_title () && vfill > 0))
	o[Y_AXIS] += vfill;
    }

  o[Y_AXIS] = p->vsize_ - p->bottom_margin_;
  if (unsmob_stencil (p->copyright_))
    o[Y_AXIS] -= unsmob_stencil (p->copyright_)->extent (Y_AXIS).length ();
  if (unsmob_stencil (p->tagline_))
    o[Y_AXIS] -= unsmob_stencil (p->tagline_)->extent (Y_AXIS).length ();
  if (unsmob_stencil (p->footer_))
    o[Y_AXIS] -= unsmob_stencil (p->footer_)->extent (Y_AXIS).length ();

  if (Stencil *s = unsmob_stencil (p->copyright_))
    stack_stencils (stencil, s, &o);
  if (Stencil *s = unsmob_stencil (p->tagline_))
    stack_stencils (stencil, s, &o);
  if (Stencil *s = unsmob_stencil (p->footer_))
    stack_stencils (stencil, s, &o);

  return stencil.smobbed_copy ();
}

Real
Page::text_height () const
{
  Real h = vsize_ - top_margin_ - bottom_margin_;
  if (unsmob_stencil (header_))
    h -= unsmob_stencil (header_)->extent (Y_AXIS).length () + head_sep_;
  if (unsmob_stencil (copyright_) || unsmob_stencil (tagline_) || unsmob_stencil (footer_))
    h -= foot_sep_;
  if (unsmob_stencil (copyright_))
    h -= unsmob_stencil (copyright_)->extent (Y_AXIS).length ();
  if (unsmob_stencil (tagline_))
    h -= unsmob_stencil (tagline_)->extent (Y_AXIS).length ();
  if (unsmob_stencil (footer_))
    h -= unsmob_stencil (footer_)->extent (Y_AXIS).length ();
  return h;
}

