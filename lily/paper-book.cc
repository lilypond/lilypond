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
#include "paper-line.hh"
#include "paper-score.hh"
#include "stencil.hh"

static Real const MIN_COVERAGE = 0.66;

static SCM
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

  hsize_ = paper->get_dimension (ly_symbol2scm ("hsize"));
  vsize_ = paper->get_dimension (ly_symbol2scm ("vsize"));
  top_margin_ = paper->get_dimension (ly_symbol2scm ("top-margin"));
  bottom_margin_ = paper->get_dimension (ly_symbol2scm ("bottom-margin"));
  head_sep_ = paper->get_dimension (ly_symbol2scm ("head-sep"));
  foot_sep_ = paper->get_dimension (ly_symbol2scm ("foot-sep"));
  text_width_ = paper->get_dimension (ly_symbol2scm ("linewidth"));
  left_margin_ = (hsize_ - text_width_) / 2;
  
  copyright_ = SCM_EOL;
  tagline_ = SCM_EOL;
  
  SCM make_header = ly_scheme_function ("make-header");
  SCM make_footer = ly_scheme_function ("make-footer");

  header_ = scm_call_2 (make_header, paper_->self_scm (),
			scm_int2num (number_));
  if (get_header ())
    get_header ()->align_to (Y_AXIS, UP);
    
  footer_ = scm_call_2 (make_footer, paper_->self_scm (),
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
  for (SCM s = lines_; s != SCM_EOL; s = ly_cdr (s))
    {
      SCM line = ly_car (s);
      out->output_line (line, &o,
			is_last && ly_cdr (s) != SCM_EOL && !get_copyright ()
			&& !get_tagline () && !get_footer ());
      
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
  if (get_copyright ())
    o[Y_AXIS] -= get_copyright ()->extent (Y_AXIS).length ();
  if (get_tagline ())
    o[Y_AXIS] -= get_tagline ()->extent (Y_AXIS).length ();
  if (get_footer ())
    o[Y_AXIS] -= get_footer ()->extent (Y_AXIS).length ();

  if (get_copyright ())
    out->output_line (stencil2line (get_copyright ()), &o,
		      is_last && !get_tagline () && !get_footer ());
  if (get_tagline ())
    out->output_line (stencil2line (get_tagline ()), &o,
		      is_last && !get_footer ());
  if (get_footer ())
    out->output_line (stencil2line (get_footer ()), &o, is_last);
  out->output_scheme (scm_list_2 (ly_symbol2scm ("stop-page"),
				  ly_bool2scm (is_last && !get_footer ())));
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

Paper_book::Paper_book ()
{
  copyright_ = SCM_EOL;
  tagline_ = SCM_EOL;
  
  smobify_self ();
}

Paper_book::~Paper_book ()
{
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

void
Paper_book::output (String outname)
{
  if (!papers_.size ())
    return;
    
  /* Generate all stencils to trigger font loads.  */
  Link_array<Page> *pages = this->pages ();

  Paper_def *paper = papers_[0];
  Paper_outputter *out = paper->get_paper_outputter (outname);
  out->output_header (paper, scopes (0), pages->size ());

  int page_count = pages->size ();
  for (int i = 0; i < page_count; i++)
    (*pages)[i]->output (out, i + 1 == page_count);

  out->output_scheme (scm_list_1 (ly_symbol2scm ("end-output")));

  /* Ugh.  */
  for (int i = pages->size (); i--;)
    delete pages->elem (i);
  delete pages;
  
  progress_indication ("\n");
}

SCM
Paper_book::scopes (int i)
{
  SCM scopes = SCM_EOL;
  if (headers_[i])
    scopes = scm_cons (headers_[i], scopes);
  if (global_headers_[i] && global_headers_[i] != headers_[i])
    scopes = scm_cons (global_headers_[i], scopes);
  return scopes;
}

Stencil*
Paper_book::title (int i)
{
  SCM user_title = ly_scheme_function ("user-title");
  SCM book_title = ly_scheme_function ("book-title");
  SCM score_title = ly_scheme_function ("score-title");
  SCM field = (i == 0 ? ly_symbol2scm ("bookTitle")
	       : ly_symbol2scm ("scoreTitle"));

  Stencil *title = 0;
  SCM scopes = this->scopes (i);
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

void
Paper_book::classic_output (String outname)
{
  int count = scores_.size ();
  Paper_outputter *out = papers_.top ()->get_paper_outputter (outname);
  out->output_header (papers_.top (), scopes (count - 1), 0);

  Paper_line *first = unsmob_paper_line (scm_vector_ref (scores_.top (),
							 scm_int2num (0)));
  Offset o (0, -0.5 * first->dim ()[Y_AXIS]);
  int line_count = SCM_VECTOR_LENGTH ((SCM) scores_.top ());
  for (int i = 0; i < line_count; i++)
    out->output_line (scm_vector_ref ((SCM) scores_.top (), scm_int2num (i)),
		      &o, i == line_count - 1);
  
  out->output_scheme (scm_list_1 (ly_symbol2scm ("end-output")));
  progress_indication ("\n");
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
      Stencil *title = this->title (i);
      if (title)
	height_ += title->extent (Y_AXIS).length ();

      int line_count = SCM_VECTOR_LENGTH ((SCM) scores_[i]);
      for (int j = 0; j < line_count; j++)
	{
	  SCM s = scm_vector_ref ((SCM) scores_[i], scm_int2num (j));
	  height_ += unsmob_paper_line (s)->dim ()[Y_AXIS];
	}
    }

  Paper_def *paper = papers_[0];
  SCM scopes = this->scopes (0);

  SCM make_tagline = ly_scheme_function ("make-tagline");
  tagline_ = scm_call_2 (make_tagline, paper->self_scm (), scopes);
  Real tag_height = 0;
  if (Stencil *s = unsmob_stencil (tagline_))
    tag_height = s->extent (Y_AXIS).length ();
  height_ += tag_height;

  SCM make_copyright = ly_scheme_function ("make-copyright");
  copyright_ = scm_call_2 (make_copyright, paper->self_scm (), scopes);
  Real copy_height = 0;
  if (Stencil *s = unsmob_stencil (copyright_))
    copy_height = s->extent (Y_AXIS).length ();
  height_ += copy_height;
}

SCM
Paper_book::lines ()
{
  int score_count = scores_.size ();
  SCM lines = SCM_EOL;
  for (int i = 0; i < score_count; i++)
    {
      if (Stencil *title = this->title (i))
	lines = ly_snoc (stencil2line (title, true), lines);
      lines = scm_append (scm_list_2 (lines, scm_vector_to_list (scores_[i])));
    }
  //debug helper; ughr
  int i = 0;
  for (SCM s = lines; s != SCM_EOL; s = ly_cdr (s))
    unsmob_paper_line (ly_car (s))->number_ = ++i;
  return lines;
}

Link_array<Page>*
Paper_book::pages ()
{
  init ();
  Page::page_count_ = 0;
  Paper_def *paper = papers_[0];
  Page *page = new Page (paper, 1);

  Real text_height = page->text_height ();

  Real copy_height = 0;
  if (Stencil *s = unsmob_stencil (copyright_))
    copy_height = s->extent (Y_AXIS).length ();

  Real tag_height = 0;
  if (Stencil *s = unsmob_stencil (tagline_))
    tag_height = s->extent (Y_AXIS).length ();

  SCM all = lines ();
  SCM proc = paper->get_scmvar ("page-breaking");
  SCM breaks = scm_apply_0 (proc, scm_list_n (all, scm_make_real (height_),
					    scm_make_real (text_height),
					    scm_make_real (-copy_height),
					    scm_make_real (-tag_height),
					    SCM_UNDEFINED));

  /* Copyright on first page.  */
  if (unsmob_stencil (copyright_))
    page->copyright_ = copyright_;

  Link_array<Page> *pages = new Link_array<Page>;
  int page_count = SCM_VECTOR_LENGTH ((SCM) breaks);
  int line = 1;
  for (int i = 0; i < page_count; i++)
    {
      if (i)
	page = new Page (paper, i+1);
      int next = i + 1 < page_count
	? ly_scm2int (scm_vector_ref (breaks, scm_int2num (i))) : 0;
      while ((!next && all != SCM_EOL) || line <= next)
	{
	  SCM s = ly_car (all);
	  page->lines_ = ly_snoc (s, page->lines_);
	  page->height_ += unsmob_paper_line (s)->dim ()[Y_AXIS];
	  page->line_count_++;
	  all = ly_cdr (all);
	  line++;
	}
      pages->push (page);
    }

  /* Tagline on last page.  */
  if (unsmob_stencil (tagline_))
    page->tagline_ = tagline_;

  return pages;
}

static SCM
c_ragged_page_breaks (SCM lines, Real book_height, Real text_height,
		      Real first, Real last)
{
  int page_number = 0;
  int page_count = int (book_height / text_height + 0.5);
  SCM breaks = SCM_EOL;
  Real page_height = text_height + first;
  Real h = 0;
  int number = 0;
  for (SCM s = lines; s != SCM_EOL; s = ly_cdr (s))
    {
      Paper_line *pl = unsmob_paper_line (ly_car (s));
      if (!pl->is_title () && h < page_height)
	number++;
      h += pl->dim ()[Y_AXIS];
      if (!pl->is_title () && h > page_height)
	{
	  breaks = ly_snoc (scm_int2num (number), breaks);
	  page_number++;
	  page_height = text_height + (page_number == page_count) * last;
	  h = 0;
	}
      if (ly_cdr (s) == SCM_EOL)
	breaks = ly_snoc (scm_int2num (pl->number_), breaks);
    }

  return scm_vector (breaks);
}

LY_DEFINE (ly_ragged_page_breaks, "ly:ragged-page-breaks",
	   5, 0, 0, (SCM lines, SCM book, SCM text, SCM first, SCM last),
	   "Return a vector with line numbers of page breaks.")
{
  SCM_ASSERT_TYPE (scm_pair_p (lines), lines, SCM_ARG1, __FUNCTION__, "list");
  SCM_ASSERT_TYPE (is_number (book), book, SCM_ARG2, __FUNCTION__, "real");
  SCM_ASSERT_TYPE (is_number (text), text, SCM_ARG2, __FUNCTION__, "real");
  SCM_ASSERT_TYPE (is_number (first), first, SCM_ARG2, __FUNCTION__, "real");
  SCM_ASSERT_TYPE (is_number (last), last, SCM_ARG2, __FUNCTION__, "real");

  return c_ragged_page_breaks (lines,
			       ly_scm2double (book), ly_scm2double (text),
			       ly_scm2double (first), ly_scm2double (last));
}
