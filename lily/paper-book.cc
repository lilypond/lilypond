/*
  paper-book.cc -- implement Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "ly-module.hh"
#include "main.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "paper-outputter.hh"
#include "paper-score.hh"
#include "paper-system.hh"
#include "stencil.hh"
#include "warn.hh"

#include "ly-smobs.icc"

Paper_book::Paper_book ()
{
  pages_ = SCM_BOOL_F;
  systems_ = SCM_BOOL_F;
  header_ = SCM_EOL;
  
  paper_ = 0;
  smobify_self ();
}

Paper_book::~Paper_book ()
{
}

IMPLEMENT_DEFAULT_EQUAL_P (Paper_book);
IMPLEMENT_SMOBS (Paper_book)
IMPLEMENT_TYPE_P (Paper_book, "ly:paper-book?")

SCM
Paper_book::mark_smob (SCM smob)
{
  Paper_book *b = (Paper_book*) SCM_CELL_WORD_1 (smob);
  for (int i = 0; i < b->score_systems_.size (); i++)
    b->score_systems_[i].gc_mark ();

  if (b->paper_)
    scm_gc_mark (b->paper_->self_scm ());
  scm_gc_mark (b->header_);
  scm_gc_mark (b->pages_);
  return b->systems_;
}

int
Paper_book::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Paper_book *b = (Paper_book*) SCM_CELL_WORD_1 (smob);
     
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

LY_DEFINE (ly_output_formats, "ly:output-formats",
	   0, 0, 0, (),
	   "Formats passed to --format as a list of strings, "
	   "used for the output.")
{
  Array<String> output_formats = split_string (output_format_global, ',');

  SCM lst = SCM_EOL;
  int output_formats_count = output_formats.size ();
  for (int i = 0; i < output_formats_count; i ++)
    lst = scm_cons (scm_makfrom0str (output_formats[i].to_str0 ()), lst);
  
  return lst; 
}

void
Paper_book::post_processing (SCM module,
			     SCM file_name)
{
  struct
  {
    bool do_it_;
    char const *func_name_;
  } settings[] = {
    {make_tex, "convert-to-tex"},
    {make_dvi, "convert-to-dvi"},
    {make_ps, "convert-to-ps"},
    {make_pdf, "convert-to-pdf"},
    {make_png, "convert-to-png"},
    {0, 0},
  };

  for (int i = 0; settings[i].func_name_; i++)
    {
      if (settings[i].do_it_)
	{
	  SCM func = scm_c_module_lookup (module, settings[i].func_name_);
	  if (scm_variable_p (func) == SCM_BOOL_T)
	    {
	      func = scm_variable_ref (func);
	      if (ly_c_procedure_p (func))
		scm_call_2 (func, self_scm (), file_name);
	    }
	}
    }
}

void
Paper_book::output (String outname)
{
  if (!score_systems_.size ())
    return;

  /* Generate all stencils to trigger font loads.  */
  pages ();
  
  SCM formats = ly_output_formats ();
  for (SCM s = formats; scm_is_pair (s); s = scm_cdr (s)) 
    {
      String format = ly_scm2string (scm_car (s));
      String file_name = outname;
      
      if (file_name != "-")
	file_name += "." + format;
      
      Paper_outputter *out = get_paper_outputter (file_name, format);
  
      SCM scopes = SCM_EOL;
      if (ly_c_module_p (header_))
	scopes = scm_cons (header_, scopes);
  
      String mod_nm = "scm framework-" + format;

      SCM mod = scm_c_resolve_module (mod_nm.to_str0 ());
      if (make_pages)
	{
	  SCM func = scm_c_module_lookup (mod, "output-framework");

	  func = scm_variable_ref (func);
	  scm_apply_0 (func, scm_list_n (out->self_scm (),
					 self_scm (),
					 scopes,
					 dump_fields (),
					 scm_makfrom0str (outname.to_str0 ()),
					 SCM_UNDEFINED));
	  out->close ();
	  scm_gc_unprotect_object (out->self_scm ());
	  post_processing (mod, scm_makfrom0str (file_name.to_str0 ()));
	}
      
      if (make_preview)
	{
	  String file_name = outname + ".preview." + format;
	  Paper_outputter *out = get_paper_outputter (file_name, format);
  
   	  SCM func = scm_c_module_lookup (mod, "output-preview-framework");
	  func = scm_variable_ref (func);
	  scm_apply_0 (func, scm_list_n (out->self_scm (),
					 self_scm (),
					 scopes,
					 dump_fields (),
					 scm_makfrom0str (outname.to_str0 ()),
					 SCM_UNDEFINED));

	  out->close ();
	  scm_gc_unprotect_object (out->self_scm ());

	  post_processing (mod, scm_makfrom0str (file_name.to_str0 ()));
     	}
    }
  progress_indication ("\n");
}

void
Paper_book::classic_output (String outname)
{
  /* Generate all stencils to trigger font loads.  */
  systems ();

  // ugh code dup
  SCM scopes = SCM_EOL;
  if (ly_c_module_p (header_))
    scopes = scm_cons (header_, scopes);

  if (ly_c_module_p (score_systems_[0].header_))
    scopes = scm_cons (score_systems_[0].header_, scopes);
  //end ugh

  Array<String> output_formats = split_string (output_format_global, ',');

  for (int i = 0; i < output_formats.size (); i++)
    {
      String format = output_formats[i];
      String mod_nm = "scm framework-" + format;
      
      SCM mod = scm_c_resolve_module (mod_nm.to_str0 ());
      SCM func = scm_c_module_lookup (mod, "output-classic-framework");

      func = scm_variable_ref (func);
      
      Paper_outputter *out = get_paper_outputter (outname + "." + format,
						  format);

      scm_apply_0 (func, scm_list_n (out->self_scm (), self_scm (), scopes,
				     dump_fields (),
				     scm_makfrom0str (outname.to_str0 ()),
				     SCM_UNDEFINED));

      scm_gc_unprotect_object (out->self_scm ());
      progress_indication ("\n");
    }
}

LY_DEFINE (ly_paper_book_pages, "ly:paper-book-pages",
	  1, 0, 0, (SCM pb),
	  "Return pages in book PB.")
{
  return unsmob_paper_book(pb)->pages ();
}

LY_DEFINE (ly_paper_book_scopes, "ly:paper-book-scopes",
	  1, 0, 0, (SCM book),
	  "Return pages in layout book @var{book}.")
{
  Paper_book *pb = unsmob_paper_book(book);
  SCM_ASSERT_TYPE(pb, book, SCM_ARG1, __FUNCTION__, "Paper_book");
  
  SCM scopes = SCM_EOL;
  if (ly_c_module_p (pb->header_))
    scopes = scm_cons (pb->header_, scopes);
  
  return scopes;
}

LY_DEFINE (ly_paper_book_systems, "ly:paper-book-systems",
	   1, 0, 0, (SCM pb),
	   "Return systems in book PB.")
{
  return unsmob_paper_book (pb)->systems ();
}

LY_DEFINE (ly_paper_book_paper, "ly:paper-book-paper",
	  1, 0, 0, (SCM pb),
	  "Return pages in book PB.")
{
  return unsmob_paper_book (pb)->paper_->self_scm ();
}

/* TODO: resurrect more complex user-tweaks for titling?  */
Stencil
Paper_book::book_title ()
{
  SCM title_func = paper_->lookup_variable (ly_symbol2scm ("book-title"));
  Stencil title;

  SCM scopes = SCM_EOL;
  if (ly_c_module_p (header_))
    scopes = scm_cons (header_, scopes);

 
  SCM tit = SCM_EOL;
  if (ly_c_procedure_p (title_func))
    tit = scm_call_2 (title_func,
		     paper_->self_scm (),
		     scopes);

  if (unsmob_stencil (tit))
    title = *unsmob_stencil (tit);

  if (!title.is_empty ())
    title.align_to (Y_AXIS, UP);
  
  return title;
}

Stencil
Paper_book::score_title (int i)
{
  SCM title_func = paper_->lookup_variable (ly_symbol2scm ("score-title"));

  Stencil title;

  // ugh code dup
  SCM scopes = SCM_EOL;
  if (ly_c_module_p (header_))
    scopes = scm_cons (header_, scopes);

  if (ly_c_module_p (score_systems_[i].header_))
    scopes = scm_cons (score_systems_[i].header_, scopes);
  //end ugh

  SCM tit = SCM_EOL;
  if (ly_c_procedure_p (title_func))
    tit = scm_call_2 (title_func,
		     paper_->self_scm (),
		     scopes);

  if (unsmob_stencil (tit))
    title = *unsmob_stencil (tit);

  if (!title.is_empty ())
    title.align_to (Y_AXIS, UP);
  
  return title;
}
  
SCM
Paper_book::systems ()
{
  if (systems_ != SCM_BOOL_F)
    return systems_;

  systems_ = SCM_EOL;
  Stencil title = book_title ();

  if (!title.is_empty ())
    {
      Paper_system *ps = new Paper_system (title, true);
      systems_ = scm_cons (ps->self_scm (), systems_);
      scm_gc_unprotect_object (ps->self_scm ());
    }
  
  int score_count = score_systems_.size ();
  for (int i = 0; i < score_count; i++)
    {
      Stencil title = score_title (i);      
      if (!title.is_empty ())
	{
	  Paper_system *ps = new Paper_system (title, true);
	  systems_ = scm_cons (ps->self_scm (), systems_);
	  scm_gc_unprotect_object (ps->self_scm ());
  	}
      
      if (scm_vector_p (score_systems_[i].systems_) == SCM_BOOL_T)
	{
	  // guh.	  
	  SCM system_list = scm_vector_to_list (score_systems_[i].systems_);

	  system_list = scm_reverse (system_list);
	  systems_ = scm_append (scm_list_2 (system_list, systems_));
	}
    }
  
  systems_ = scm_reverse (systems_);
  
  int i = 0;
  Paper_system *last = 0;
  for (SCM s = systems_; s != SCM_EOL; s = scm_cdr (s))
    {
      Paper_system *ps = unsmob_paper_system (scm_car (s));
      ps->number_ = ++i;

      if (last && last->is_title ())
	// ugh, hardcoded.	
	ps->penalty_ = 10000;
      last = ps;
    }
  
  return systems_;
}

SCM
Paper_book::pages ()
{
  if (SCM_BOOL_F != pages_)
    return pages_;

  pages_ = SCM_EOL;
  SCM proc = paper_->c_variable ("page-breaking");
  pages_ = scm_apply_0 (proc, scm_list_2 (systems (), self_scm ()));
  return pages_;
}


/****************************************************************/

Score_systems::Score_systems ()
{
  systems_ = SCM_EOL;
  header_ = SCM_EOL;
}

void
Score_systems::gc_mark ()
{
  scm_gc_mark (systems_);
  scm_gc_mark (header_);
}

