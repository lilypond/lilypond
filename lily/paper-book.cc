/*
  paper-book.cc -- implement Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "paper-book.hh"

#include "grob.hh"
#include "main.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "paper-system.hh"
#include "text-interface.hh"
#include "warn.hh"
#include "program-option.hh"
#include "page-marker.hh"

#include "ly-smobs.icc"

Paper_book::Paper_book ()
{
  header_ = SCM_EOL;
  header_0_ = SCM_EOL;
  pages_ = SCM_BOOL_F;
  scores_ = SCM_EOL;
  performances_ = SCM_EOL;
  systems_ = SCM_BOOL_F;

  paper_ = 0;
  smobify_self ();
}

Paper_book::~Paper_book ()
{
}

IMPLEMENT_DEFAULT_EQUAL_P (Paper_book);
IMPLEMENT_SMOBS (Paper_book);
IMPLEMENT_TYPE_P (Paper_book, "ly:paper-book?");

SCM
Paper_book::mark_smob (SCM smob)
{
  Paper_book *b = (Paper_book *) SCM_CELL_WORD_1 (smob);
  if (b->paper_)
    scm_gc_mark (b->paper_->self_scm ());
  scm_gc_mark (b->header_);
  scm_gc_mark (b->header_0_);
  scm_gc_mark (b->pages_);
  scm_gc_mark (b->performances_);
  scm_gc_mark (b->scores_);
  return b->systems_;
}

int
Paper_book::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Paper_book *b = (Paper_book *) SCM_CELL_WORD_1 (smob);
  (void)b;
  scm_puts ("#<Paper_book>", port);
  return 1;
}

SCM
dump_fields ()
{
  SCM fields = SCM_EOL;
  for (vsize i = dump_header_fieldnames_global.size (); i--;)
    fields
      = scm_cons (ly_symbol2scm (dump_header_fieldnames_global[i].c_str ()),
		  fields);
  return fields;
}

void
Paper_book::add_score (SCM s)
{
  scores_ = scm_cons (s, scores_);
}

void
Paper_book::add_performance (SCM s)
{
  performances_ = scm_cons (s, performances_);
}

void
Paper_book::output (SCM output_channel)
{
  if (scm_is_pair (performances_))
    {
      SCM proc = ly_lily_module_constant ("write-performances-midis");
 
      scm_call_2 (proc, performances (), output_channel);
    }

  if (scores_ == SCM_EOL)
    return;

  /* Generate all stencils to trigger font loads.  */
  pages ();

  SCM scopes = SCM_EOL;
  if (ly_is_module (header_))
    scopes = scm_cons (header_, scopes);

  string mod_nm = "scm framework-" + get_output_backend_name ();

  SCM mod = scm_c_resolve_module (mod_nm.c_str ());

  if (get_program_option ("print-pages"))
    {
      SCM func = scm_c_module_lookup (mod, "output-framework");

      func = scm_variable_ref (func);
      scm_apply_0 (func, scm_list_n (output_channel,
				     self_scm (),
				     scopes,
				     dump_fields (),
				     SCM_UNDEFINED));
    }

  if (get_program_option ("preview"))
    {
      SCM func = scm_c_module_lookup (mod, "output-preview-framework");
      func = scm_variable_ref (func);
      scm_apply_0 (func, scm_list_n (output_channel,
				     self_scm (),
				     scopes,
				     dump_fields (),
				     SCM_UNDEFINED));
    }
}

void
Paper_book::classic_output (SCM output)
{
  if (scm_is_pair (performances_))
    {
      SCM proc = ly_lily_module_constant ("write-performances-midis");
 
      scm_call_2 (proc, performances (), output);
    }
  
  /* Generate all stencils to trigger font loads.  */
  systems ();

  SCM scopes = SCM_EOL;
  if (ly_is_module (header_))
    scopes = scm_cons (header_, scopes);

  if (ly_is_module (header_0_))
    scopes = scm_cons (header_0_, scopes);

  string format = get_output_backend_name ();
  string mod_nm = "scm framework-" + format;

  SCM mod = scm_c_resolve_module (mod_nm.c_str ());
  SCM func = scm_c_module_lookup (mod, "output-classic-framework");

  func = scm_variable_ref (func);
  scm_apply_0 (func, scm_list_n (output,
				 self_scm (),
				 scopes,
				 dump_fields (),
				 SCM_UNDEFINED));

  progress_indication ("\n");
}

/* TODO: resurrect more complex user-tweaks for titling?  */
Stencil
Paper_book::book_title ()
{
  SCM title_func = paper_->lookup_variable (ly_symbol2scm ("book-title"));
  Stencil title;

  SCM scopes = SCM_EOL;
  if (ly_is_module (header_))
    scopes = scm_cons (header_, scopes);

  SCM tit = SCM_EOL;
  if (ly_is_procedure (title_func))
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
Paper_book::score_title (SCM header)
{
  SCM title_func = paper_->lookup_variable (ly_symbol2scm ("score-title"));

  Stencil title;

  SCM scopes = SCM_EOL;
  if (ly_is_module (header_))
    scopes = scm_cons (header_, scopes);

  if (ly_is_module (header))
    scopes = scm_cons (header, scopes);

  SCM tit = SCM_EOL;
  if (ly_is_procedure (title_func))
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
set_page_permission (SCM sys, SCM symbol, SCM permission)
{
  if (Paper_score *ps = dynamic_cast<Paper_score*> (unsmob_music_output (sys)))
    {
      vector<Grob*> cols = ps->get_columns ();
      if (cols.size ())
	{
	  Paper_column *col = dynamic_cast<Paper_column*> (cols.back ());
	  col->set_property (symbol, permission);
	  col->find_prebroken_piece (LEFT)->set_property (symbol, permission);
	}
    }
  else if (Prob *pb = unsmob_prob (sys))
    pb->set_property (symbol, permission);
}

/* read the breakbefore property of a score block and set up the preceding
   system-spec to honour it. That is, SYS should be the system spec that
   immediately precedes the score (from which HEADER is taken)
   in the get_system_specs () list */
void
set_system_penalty (SCM sys, SCM header)
{
  if (ly_is_module (header))
    {
      SCM force = ly_module_lookup (header, ly_symbol2scm ("breakbefore"));
      if (SCM_VARIABLEP (force)
	  && scm_is_bool (SCM_VARIABLE_REF (force)))
	{
	  if (to_boolean (SCM_VARIABLE_REF (force)))
	    {
	      set_page_permission (sys, ly_symbol2scm ("page-break-permission"),
				   ly_symbol2scm ("force"));
	      set_page_permission (sys, ly_symbol2scm ("line-break-permission"),
				   ly_symbol2scm ("force"));
	    }
	  else
	    set_page_permission (sys, ly_symbol2scm ("page-break-permission"), SCM_EOL);
	}
    }
}

SCM
Paper_book::get_score_title (SCM header)
{
  Stencil title = score_title (header);
  if (title.is_empty ())
    title = score_title (header_);
  if (!title.is_empty ())
    {
      /*
	TODO: this should come from the \layout {} block, which should
	override settings from \paper {}
      */
      SCM props = paper_->lookup_variable (ly_symbol2scm ("score-title-properties"));
      Prob *ps = make_paper_system (props);
      paper_system_set_stencil (ps, title);

      return ps->self_scm ();
    }

  return SCM_BOOL_F;
}


SCM
Paper_book::get_system_specs ()
{
  SCM system_specs = SCM_EOL;
  
  Stencil title = book_title ();
  if (!title.is_empty ())
    {
      SCM props = paper_->lookup_variable (ly_symbol2scm ("book-title-properties"));
      Prob *ps = make_paper_system (props);
      paper_system_set_stencil (ps, title);

      system_specs = scm_cons (ps->self_scm (), system_specs);
      ps->unprotect ();
    }

  SCM page_properties
    = scm_call_1 (ly_lily_module_constant ("layout-extract-page-properties"),
		  paper_->self_scm ());

  SCM header = SCM_EOL;
  for (SCM s = scm_reverse (scores_); scm_is_pair (s); s = scm_cdr (s))
    {
      if (ly_is_module (scm_car (s)))
	{
	  header = scm_car (s);
	  if (header_0_ == SCM_EOL)
	    header_0_ = header;
	}
      else if (Page_marker *page_marker = unsmob_page_marker (scm_car (s)))
	{
	  /* a page marker: set previous element page break or turn permission */
	  if (scm_is_pair (system_specs))
	    set_page_permission (scm_car (system_specs),
				 page_marker->permission_symbol (),
				 page_marker->permission_value ());
	}
      else if (Music_output *mop = unsmob_music_output (scm_car (s)))
	{
	  if (Paper_score *pscore = dynamic_cast<Paper_score *> (mop))
	    {
	      SCM title = get_score_title (header);

	      if (scm_is_pair (system_specs))
		set_system_penalty (scm_car (system_specs), header);

	      if (unsmob_prob (title))
		{
		  system_specs = scm_cons (title, system_specs);
		  unsmob_prob (title)->unprotect ();
		}

	      header = SCM_EOL;
	      system_specs = scm_cons (pscore->self_scm (), system_specs);
	    }
	  else
	    {
	      /*
		Ignore MIDI
	      */
	    }
	}
      else if (Text_interface::is_markup (scm_car (s)))
	{
	  SCM t = Text_interface::interpret_markup (paper_->self_scm (),
						    page_properties,
						    scm_car (s));
	  
	  // TODO: init props
	  Prob *ps = make_paper_system (SCM_EOL);
	  ps->set_property ("page-break-permission", ly_symbol2scm ("allow"));
	  ps->set_property ("page-turn-permission", ly_symbol2scm ("allow"));
	  
	  paper_system_set_stencil (ps, *unsmob_stencil (t));
	  ps->set_property ("is-title", SCM_BOOL_T); 
	  system_specs = scm_cons (ps->self_scm (), system_specs);
	  ps->unprotect ();
	  
	  // FIXME: figure out penalty.
	  //set_system_penalty (ps, scores_[i].header_);
	}
      else
	assert (0);
    }

  system_specs = scm_reverse_x (system_specs, SCM_EOL);
  return system_specs;
}

SCM
Paper_book::systems ()
{
  if (systems_ != SCM_BOOL_F)
    return systems_;

  systems_ = SCM_EOL;
  SCM specs = get_system_specs ();
  for (SCM s = specs; scm_is_pair (s); s = scm_cdr (s))
    {
      if (Paper_score *pscore = dynamic_cast<Paper_score*> (unsmob_music_output (scm_car (s))))
	{
	  SCM system_list = scm_vector_to_list (pscore->get_paper_systems ());
	  system_list = scm_reverse (system_list);
	  systems_ = scm_append (scm_list_2 (system_list, systems_));
	}
      else
	{
	  systems_ = scm_cons (scm_car (s), systems_);
	}
    }
  
  systems_ = scm_reverse (systems_);

  /* backwards compatibility for the old page breaker */
  int i = 0;
  Prob *last = 0;
  for (SCM s = systems_; scm_is_pair (s); s = scm_cdr (s))
    {
      Prob *ps = unsmob_prob (scm_car (s));
      ps->set_property ("number", scm_from_int (++i));

      if (last
	  && to_boolean (last->get_property ("is-title"))
	  && !scm_is_number (ps->get_property ("penalty")))
	ps->set_property ("penalty", scm_from_int (10000));
      last = ps;

      if (scm_is_pair (scm_cdr (s)))
	{
	  SCM perm = ps->get_property ("page-break-permission");
	  Prob *next = unsmob_prob (scm_cadr (s));
	  if (perm == SCM_EOL)
	    next->set_property ("penalty", scm_from_int (10001));
	  else if (perm == ly_symbol2scm ("force"))
	    next->set_property ("penalty", scm_from_int (-10001));
	}
    }

  return systems_;
}

SCM
Paper_book::pages ()
{
  if (SCM_BOOL_F != pages_)
    return pages_;

  pages_ = SCM_EOL;
  SCM proc = paper_->c_variable ("page-breaking-wrapper");
  pages_ = scm_apply_0 (proc, scm_list_1 (self_scm ()));

  /* set systems_ from the pages */
  if (systems_ == SCM_BOOL_F)
    {
      systems_ = SCM_EOL;
      for (SCM p = pages_; scm_is_pair (p); p = scm_cdr (p))
	{
	  Prob *page = unsmob_prob (scm_car (p));
	  SCM systems = page->get_property ("lines");

	  systems_ = scm_append (scm_list_2 (systems_, systems));
	}
    }

  return pages_;
}

SCM
Paper_book::performances () const
{
  return scm_reverse (performances_);
}
