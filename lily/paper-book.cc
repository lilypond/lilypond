/*
  paper-book.cc -- implement Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "paper-book.hh"

#include "ly-module.hh"
#include "main.hh"
#include "output-def.hh"
#include "paper-outputter.hh"
#include "paper-score.hh"
#include "paper-system.hh"
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

void
Paper_book::output (String outname)
{
  if (!score_systems_.size ())
    return;

  /* Generate all stencils to trigger font loads.  */
  pages ();
  
  SCM scopes = SCM_EOL;
  if (ly_c_module_p (header_))
    scopes = scm_cons (header_, scopes);
  
  String mod_nm = "scm framework-" + output_backend_global;

  SCM mod = scm_c_resolve_module (mod_nm.to_str0 ());
  if (make_pages)
    {
      SCM func = scm_c_module_lookup (mod, "output-framework");

      func = scm_variable_ref (func);
      scm_apply_0 (func, scm_list_n (scm_makfrom0str (outname.to_str0 ()),
				     self_scm (),
				     scopes,
				     dump_fields (),
				     SCM_UNDEFINED));
    }
      
  if (make_preview)
    {
      SCM func = scm_c_module_lookup (mod, "output-preview-framework");
      func = scm_variable_ref (func);
      scm_apply_0 (func, scm_list_n (scm_makfrom0str (outname.to_str0 ()),
				     self_scm (),
				     scopes,
				     dump_fields (),
				     SCM_UNDEFINED));
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

  String format = output_backend_global;
  String mod_nm = "scm framework-" + format;
      
  SCM mod = scm_c_resolve_module (mod_nm.to_str0 ());
  SCM func = scm_c_module_lookup (mod, "output-classic-framework");

  func = scm_variable_ref (func);
      
  scm_apply_0 (func, scm_list_n (scm_makfrom0str (outname.to_str0 ()),
				 self_scm (),
				 scopes,
				 dump_fields (),
				 SCM_UNDEFINED));

  progress_indication ("\n");
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

void
set_system_penalty (Paper_system * ps, SCM header)
{
  if (ly_c_module_p (header))
    {
      SCM force = ly_module_lookup (header, ly_symbol2scm ("breakbefore"));
      if (SCM_VARIABLEP(force)
	  && scm_is_bool (SCM_VARIABLE_REF(force)))
	{
	  ps->break_before_penalty_ = to_boolean (SCM_VARIABLE_REF(force))
	    ? -10000
	    : 10000;
	}
    }
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
      set_system_penalty (ps, header_);
      
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

	  set_system_penalty (ps, score_systems_[i].header_);
	  
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

      if (last
	  && last->is_title ()
	  && !ps->break_before_penalty_)
	ps->break_before_penalty_ = 10000;
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

