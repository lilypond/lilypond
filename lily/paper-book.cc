/*
  paper-book.cc -- implement Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "ly-module.hh"
#include "main.hh"
#include "page.hh"
#include "paper-book.hh"
#include "output-def.hh"
#include "paper-outputter.hh"
#include "paper-line.hh"
#include "paper-score.hh"
#include "stencil.hh"
#include "warn.hh"


// JUNKME
SCM
stencil2line (Stencil stil, bool is_title = false)
{
  static SCM z;
  if (!z)
    z = scm_permanent_object (ly_offset2scm (Offset (0, 0)));
  Offset dim = Offset (stil.extent (X_AXIS).length (),
		       stil.extent (Y_AXIS).length ());
  Paper_line *pl = new Paper_line (dim, scm_cons (stil.smobbed_copy (),
						  SCM_EOL),
				   -10001 * is_title, is_title);

  return scm_gc_unprotect_object (pl->self_scm ());
}


Paper_book::Paper_book ()
{
  pages_ = SCM_EOL;
  lines_ = SCM_EOL;
  copyright_ = SCM_EOL;
  tagline_ = SCM_EOL;
  header_ = SCM_EOL;
  
  bookpaper_ = 0;
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
  Paper_book *b = (Paper_book*) SCM_CELL_WORD_1 (smob);
  for (int i = 0; i < b->score_lines_.size (); i++)
    b->score_lines_[i].gc_mark ();

  scm_gc_mark (b->copyright_);
  if (b->bookpaper_)
    scm_gc_mark (b->bookpaper_->self_scm ());
  scm_gc_mark (b->header_);
  scm_gc_mark (b->pages_);
  scm_gc_mark (b->lines_);
  return b->tagline_;
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

Array<String>
split_string (String s, char c)
{
  Array<String> rv; 
  while (s.length ())
    {
      int i = s.index (c);
      
      if (i == 0)
	{
	  s = s.nomid_string (0, 1);
	  continue;
	}
      
      if (i < 0)
	i = s.length () ;

      rv.push (s.cut_string (0, i));
      s = s.nomid_string (0, i);
    }

  return rv;
}

SCM
dump_fields ()
{
  SCM fields = SCM_EOL;
  for (int i = dump_header_fieldnames_global.size (); i--; )
    fields
      = scm_cons (ly_symbol2scm (dump_header_fieldnames_global[i].to_str0 ()),
		  fields);
  return fields;
}

/*
  TODO: there is too much code dup, and the interface is not
  clear. FIXME.
 */
void
Paper_book::output (String outname)
{
  if (!score_lines_.size ())
    // FIXME: no end-output?
    return;
    
  /* Generate all stencils to trigger font loads.  */
  pages ();

  Array<String> output_formats = split_string (output_format_global, ',');

  for (int i = 0; i < output_formats.size (); i++)
    {
      String format = output_formats[i];
      Paper_outputter *out = get_paper_outputter (outname + "." + output_formats[i], format);
      
      

  
      SCM scopes = SCM_EOL;
      if (ly_c_module_p (header_))
	scopes = scm_cons (header_, scopes);
  
      String func_nm = output_format_global;
      func_nm = "output-framework-" + func_nm;
	
      SCM func = ly_scheme_function (func_nm.to_str0 ());
      scm_apply_0 (func, scm_list_n (out->self_scm (),
				     self_scm (),
				     scopes,
				     dump_fields (),
				     scm_makfrom0str (outname.to_str0 ()),
				     SCM_UNDEFINED
				     )) ;

      scm_gc_unprotect_object (out->self_scm ());
    }
}


void
Paper_book::classic_output (String outname)
{
  String format = "tex";
  Paper_outputter *out = get_paper_outputter (outname + "." + format, format);

  /* Generate all stencils to trigger font loads.  */
  lines ();


  // ugh code dup
  SCM scopes = SCM_EOL;
  if (ly_c_module_p (header_))
    scopes = scm_cons (header_, scopes);

  if (ly_c_module_p (score_lines_[0].header_))
    scopes = scm_cons (score_lines_[0].header_, scopes);
  //end ugh
  
  String func_nm = output_format_global;
  func_nm = "output-classic-framework-" + func_nm;
      
  SCM func = ly_scheme_function (func_nm.to_str0 ());
  scm_apply_0 (func, scm_list_n (out->self_scm (),
				 self_scm (),
				 scopes,
				 dump_fields (),
				 scm_makfrom0str (outname.to_str0 ()),
				 SCM_UNDEFINED
				 )) ;

  progress_indication ("\n");
}




LY_DEFINE(ly_paper_book_pages, "ly:paper-book-pages",
	  1,0,0,
	  (SCM pb),
	  "Return pages in book PB.")
{
  return unsmob_paper_book(pb)->pages ();
}


LY_DEFINE(ly_paper_book_lines, "ly:paper-book-lines",
	  1,0,0,
	  (SCM pb),
	  "Return lines in book PB.")
{
  return unsmob_paper_book (pb)->lines ();
}


LY_DEFINE(ly_paper_book_book_paper, "ly:paper-book-book-paper",
	  1,0,0,
	  (SCM pb),
	  "Return pages in book PB.")
{
  return unsmob_paper_book(pb)->bookpaper_->self_scm ();
}

Stencil
Paper_book::title (int i)
{
  SCM user_title = bookpaper_->lookup_variable (ly_symbol2scm ("user-title"));
  SCM book_title = bookpaper_->lookup_variable (ly_symbol2scm ("book-title"));
  SCM score_title = bookpaper_->lookup_variable (ly_symbol2scm ("score-title"));
  SCM field = (i == 0 ? ly_symbol2scm ("bookTitle")
	       : ly_symbol2scm ("scoreTitle"));

  Stencil title;

  // ugh code dup
  SCM scopes = SCM_EOL;
  if (ly_c_module_p (header_))
    scopes = scm_cons (header_, scopes);

  if (ly_c_module_p (score_lines_[i].header_))
    scopes = scm_cons (score_lines_[i].header_, scopes);
   //end ugh
  
  SCM s = ly_modules_lookup (scopes, field, SCM_BOOL_F);
  if (s != SCM_BOOL_F)
    title = *unsmob_stencil (scm_call_2 (user_title,
					 bookpaper_->self_scm (),
					 s));
  else
    title = *unsmob_stencil (scm_call_2 (i == 0 ? book_title : score_title,
					 bookpaper_->self_scm (),
					 scopes));
  if (!title.is_empty ())
    title.align_to (Y_AXIS, UP);
  
  return title;
}

/* calculate book height, #lines, stencils.  */
void
Paper_book::init ()
{
  int score_count = score_lines_.size ();

  /* Calculate the full book height.  Hmm, can't we cache system
     heights while making stencils?  */
  height_ = 0;
  for (int i = 0; i < score_count; i++)
    {
      Stencil title = this->title (i);
      if (!title.is_empty ())
	height_ += title.extent (Y_AXIS).length ();

      int line_count = SCM_VECTOR_LENGTH (score_lines_[i].lines_);
      for (int j = 0; j < line_count; j++)
	{
	  SCM s = scm_vector_ref ((SCM) score_lines_[i].lines_, scm_int2num (j));
	  height_ += unsmob_paper_line (s)->dim ()[Y_AXIS];
	}
    }

  Output_def *paper = bookpaper_;
  
  SCM scopes = SCM_EOL;
  if (ly_c_module_p (header_))
    scopes = scm_cons (header_, scopes);

  SCM make_tagline = paper->c_variable ("make-tagline");
  tagline_ = scm_call_2 (make_tagline, paper->self_scm (), scopes);
  Real tag_height = 0;
  if (Stencil *s = unsmob_stencil (tagline_))
    tag_height = s->extent (Y_AXIS).length ();
  height_ += tag_height;

  SCM make_copyright = paper->c_variable ("make-copyright");
  copyright_ = scm_call_2 (make_copyright, paper->self_scm (), scopes);
  Real copy_height = 0;
  if (Stencil *s = unsmob_stencil (copyright_))
    copy_height = s->extent (Y_AXIS).length ();
  height_ += copy_height;
}

SCM
Paper_book::lines ()
{
  if (ly_c_pair_p (lines_))
    return lines_;
      
  int score_count = score_lines_.size ();
  for (int i = 0; i < score_count; i++)
    {
      Stencil title = this->title (i);      
      if (!title.is_empty ())
	lines_ = scm_cons (stencil2line (title, true), lines_);

      lines_ = scm_append (scm_list_2 (scm_vector_to_list (score_lines_[i].lines_), lines_));
    }
  
  lines_ = scm_reverse (lines_);

  int i = 0;
  for (SCM s = lines_; s != SCM_EOL; s = ly_cdr (s))
    unsmob_paper_line (ly_car (s))->number_ = ++i;
  return lines_;
}

SCM
Paper_book::pages ()
{
  if (ly_c_pair_p (pages_))
    return pages_;

  init ();
  Output_def *paper = bookpaper_;
  Page *page = new Page (paper, 1);

  Real text_height = page->text_height ();

  Real copy_height = 0;
  if (Stencil *s = unsmob_stencil (copyright_))
    copy_height = s->extent (Y_AXIS).length ();

  Real tag_height = 0;
  if (Stencil *s = unsmob_stencil (tagline_))
    tag_height = s->extent (Y_AXIS).length ();

  SCM all = lines ();
  SCM proc = paper->c_variable ("page-breaking");
  SCM breaks = scm_apply_0 (proc, scm_list_n (all, scm_make_real (height_),
					      scm_make_real (text_height),
					      scm_make_real (-copy_height),
					      scm_make_real (-tag_height),
					      SCM_UNDEFINED));

  /* Copyright on first page.  */
  if (unsmob_stencil (copyright_))
    page->copyright_ = copyright_;

  int page_count = SCM_VECTOR_LENGTH ((SCM) breaks);
  int line = 1;

  for (int i = 0; i < page_count; i++)
    {
      if (i)
	page = new Page (paper, i + 1);

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
      if (i == page_count-1)
	page->is_last_ = true;
      
      pages_ = scm_cons (page->self_scm (), pages_);
    }

  /* Tagline on last page.  */
  if (unsmob_stencil (tagline_))
    page->tagline_ = tagline_;

  pages_ =  scm_reverse (pages_);
  return pages_;
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
  SCM_ASSERT_TYPE (ly_c_number_p (book), book, SCM_ARG2, __FUNCTION__, "real");
  SCM_ASSERT_TYPE (ly_c_number_p (text), text, SCM_ARG2, __FUNCTION__, "real");
  SCM_ASSERT_TYPE (ly_c_number_p (first), first, SCM_ARG2, __FUNCTION__, "real");
  SCM_ASSERT_TYPE (ly_c_number_p (last), last, SCM_ARG2, __FUNCTION__, "real");

  return c_ragged_page_breaks (lines,
			       ly_scm2double (book), ly_scm2double (text),
			       ly_scm2double (first), ly_scm2double (last));
}

/****************************************************************/

Score_lines::Score_lines ()
{
  lines_ = SCM_EOL;
  header_ = SCM_EOL;
}

void
Score_lines::gc_mark ()
{
  scm_gc_mark (lines_);
  scm_gc_mark (header_);
}

