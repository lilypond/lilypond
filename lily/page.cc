/*
  page.cc -- implement Page

  source file of the GNU LilyPond music typesetter

  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "ly-module.hh"
#include "page.hh"
#include "paper-def.hh"
#include "paper-outputter.hh"
#include "paper-line.hh"
#include "stencil.hh"
#include "warn.hh"

// JUNKME
SCM
stencil2line (Stencil* stil, bool is_title = false)
{
  static SCM z;
  if (!z)
    z = scm_permanent_object (ly_offset2scm (Offset (0, 0)));
  Offset dim = Offset (stil->extent (X_AXIS).length (),
		       stil->extent (Y_AXIS).length ());
  Paper_line *pl = new Paper_line (dim, scm_cons (stil->smobbed_copy (),
						  SCM_EOL),
				   -10001 * is_title, is_title);

  return scm_gc_unprotect_object (pl->self_scm ());
}

static Real const MIN_COVERAGE = 0.66;

int Page::page_count_ = 0;

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

  if (unsmob_stencil (header_))
    {
      out->output_line (stencil2line (unsmob_stencil (header_)), &o, false);
      o[Y_AXIS] += head_sep_;
    }
  for (SCM s = lines_; s != SCM_EOL; s = ly_cdr (s))
    {
      SCM line = ly_car (s);
      out->output_line (line, &o,
			is_last && ly_cdr (s) != SCM_EOL && !unsmob_stencil (copyright_)
			&& !unsmob_stencil (tagline_) && !unsmob_stencil (footer_));
      
      /* Do not put vfill between title and its music, */
      if (scm_pair_p (ly_cdr (s))
	  && (!unsmob_paper_line (line)->is_title () || vfill < 0))
	o[Y_AXIS] += vfill;
      /* rather put extra just before the title.  */
      if (ly_cdr (s) != SCM_EOL
	  && (unsmob_paper_line (ly_cadr (s))->is_title () && vfill > 0))
	o[Y_AXIS] += vfill;
    }

  o[Y_AXIS] = vsize_ - bottom_margin_;
  if (unsmob_stencil (copyright_))
    o[Y_AXIS] -= unsmob_stencil (copyright_)->extent (Y_AXIS).length ();
  if (unsmob_stencil (tagline_))
    o[Y_AXIS] -= unsmob_stencil (tagline_)->extent (Y_AXIS).length ();
  if (unsmob_stencil (footer_))
    o[Y_AXIS] -= unsmob_stencil (footer_)->extent (Y_AXIS).length ();

  if (unsmob_stencil (copyright_))
    out->output_line (stencil2line (unsmob_stencil (copyright_)), &o,
		      is_last && !unsmob_stencil (tagline_) && !unsmob_stencil (footer_));
  if (unsmob_stencil (tagline_))
    out->output_line (stencil2line (unsmob_stencil (tagline_)), &o,
		      is_last && !unsmob_stencil (footer_));
  if (unsmob_stencil (footer_))
    out->output_line (stencil2line (unsmob_stencil (footer_)), &o, is_last);
  out->output_scheme (scm_list_2 (ly_symbol2scm ("stop-page"),
				  ly_bool2scm (is_last && !unsmob_stencil (footer_))));
  progress_indication ("]");
}

Real
Page::text_height ()
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
