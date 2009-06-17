/*
  paper-book.cc -- implement Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "paper-book.hh"

#include "grob.hh"
#include "international.hh"
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
  bookparts_ = SCM_EOL;
  performances_ = SCM_EOL;
  systems_ = SCM_BOOL_F;

  paper_ = 0;
  parent_ = 0;
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
  if (b->parent_)
    scm_gc_mark (b->parent_->self_scm ());
  scm_gc_mark (b->header_);
  scm_gc_mark (b->header_0_);
  scm_gc_mark (b->pages_);
  scm_gc_mark (b->performances_);
  scm_gc_mark (b->scores_);
  scm_gc_mark (b->bookparts_);
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

Output_def *
Paper_book::top_paper ()
{
  Output_def *paper = paper_;
  while (paper->parent_)
    paper = paper->parent_;
  return paper;
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
Paper_book::add_bookpart (SCM p)
{
  bookparts_ = scm_cons (p, bookparts_);
}

void
Paper_book::add_performance (SCM s)
{
  performances_ = scm_cons (s, performances_);
}

int
Paper_book::output_aux (SCM output_channel,
			bool is_last,
			int *first_page_number,
			int *first_performance_number)
{
  int page_nb = 0;
  if (scm_is_pair (performances_))
    {
      SCM proc = ly_lily_module_constant ("write-performances-midis");
 
      scm_call_3 (proc,
		  performances (),
		  output_channel,
		  scm_long2num (*first_performance_number));
      *first_performance_number += scm_ilength (performances_);
    }

  if (scm_is_pair (bookparts_))
    {
      for (SCM p = bookparts_; scm_is_pair (p); p = scm_cdr (p))
	if (Paper_book *pbookpart = unsmob_paper_book (scm_car (p)))
	  {
	    bool is_last_part = (is_last && !scm_is_pair (scm_cdr (p)));
	    page_nb += pbookpart->output_aux (output_channel,
					      is_last_part,
					      first_page_number,
					      first_performance_number);
	  }
    }
  else
    {
      if (scores_ == SCM_EOL)
	return 0;
      paper_->set_variable (ly_symbol2scm ("first-page-number"),
			    scm_long2num (*first_page_number));
      paper_->set_variable (ly_symbol2scm ("is-last-bookpart"),
			    ly_bool2scm (is_last));
      /* Generate all stencils to trigger font loads.  */
      page_nb = scm_ilength (pages ());
      *first_page_number += page_nb;
    }
  return page_nb;
}

void
Paper_book::output (SCM output_channel)
{
  int first_page_number = robust_scm2int (paper_->c_variable ("first-page-number"), 1);
  int first_performance_number = 0;
  if (!output_aux (output_channel,
		   true,
		   &first_page_number,
		   &first_performance_number))
    return;
      
  SCM scopes = SCM_EOL;
  if (ly_is_module (header_))
    scopes = scm_cons (header_, scopes);

  string mod_nm = "scm framework-" + get_output_backend_name ();

  SCM mod = scm_c_resolve_module (mod_nm.c_str ());

  if (get_program_option ("print-pages"))
    {
      SCM framework = ly_module_lookup (mod, ly_symbol2scm ("output-framework"));

      if (framework != SCM_BOOL_F)
	{
	  SCM func = scm_variable_ref (framework);
	  scm_apply_0 (func, scm_list_n (output_channel,
					 self_scm (),
					 scopes,
					 dump_fields (),
					 SCM_UNDEFINED));
	}
      else
	warning (_f ("program option -dprint-pages not supported by backend `%s'",
		     get_output_backend_name ()));
    }

  if (get_program_option ("preview"))
    {
      SCM framework = ly_module_lookup (mod, ly_symbol2scm ("output-preview-framework"));

      if (framework != SCM_BOOL_F)
	{
	  SCM func = scm_variable_ref (framework);
	  scm_apply_0 (func, scm_list_n (output_channel,
					 self_scm (),
					 scopes,
					 dump_fields (),
					 SCM_UNDEFINED));
	}
      else
	warning (_f ("program option -dpreview not supported by backend `%s'",
		     get_output_backend_name ()));
    }
}

void
Paper_book::classic_output_aux (SCM output,
				int *first_performance_number)
{
  if (scm_is_pair (performances_))
    {
      SCM proc = ly_lily_module_constant ("write-performances-midis");
      scm_call_3 (proc,
		  performances (),
		  output,
		  scm_long2num (*first_performance_number));
      *first_performance_number += scm_ilength (performances_);
    }
  
  /* Generate all stencils to trigger font loads.  */
  systems ();
}

void
Paper_book::classic_output (SCM output)
{
  int first_performance_number = 0;
  classic_output_aux (output, &first_performance_number);

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

void
set_labels (SCM sys, SCM labels)
{
  if (Paper_score *ps = dynamic_cast<Paper_score*> (unsmob_music_output (sys)))
    {
      vector<Grob*> cols = ps->get_columns ();
      if (cols.size ())
	{
	  Paper_column *col = dynamic_cast<Paper_column*> (cols[0]);
	  col->set_property ("labels", 
			     scm_append_x (scm_list_2 (col->get_property ("labels"),
						       labels)));
	  Paper_column *col_right = dynamic_cast<Paper_column*> (col->find_prebroken_piece (RIGHT));
	  col_right->set_property ("labels", 
				   scm_append_x (scm_list_2 (col_right->get_property ("labels"),
							     labels)));
	}
    }
  else if (Prob *pb = unsmob_prob (sys))
    pb->set_property ("labels", 
		      scm_append_x (scm_list_2 (pb->get_property ("labels"),
						labels)));
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

  SCM interpret_markup_list = ly_lily_module_constant ("interpret-markup-list");
  SCM header = SCM_EOL;
  SCM labels = SCM_EOL;
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
	  /* page markers are used to set page breaking/turning permission,
	     or to place bookmarking labels */ 
	  if (scm_is_symbol (page_marker->permission_symbol ()))
	    {
	      /* set previous element page break or turn permission */
	      if (scm_is_pair (system_specs))
		set_page_permission (scm_car (system_specs),
				     page_marker->permission_symbol (),
				     page_marker->permission_value ());
	    }
	  if (scm_is_symbol (page_marker->label ()))
	    {
	      /* The next element label is to be set */
	      labels = scm_cons (page_marker->label (), labels);
	    }
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
	      if (scm_is_pair (labels))
		{
		  set_labels (scm_car (system_specs), labels);
		  labels = SCM_EOL;
		}
	    }
	  else
	    {
	      /*
		Ignore MIDI
	      */
	    }
	}
      else if (Text_interface::is_markup_list (scm_car (s)))
	{
	  SCM texts = scm_call_3 (interpret_markup_list,
				  paper_->self_scm (),
				  page_properties,
				  scm_car (s));
	  for (SCM list = texts ; scm_is_pair (list) ; list = scm_cdr (list))
	    {
	      SCM t = scm_car (list);
	      // TODO: init props
	      Prob *ps = make_paper_system (SCM_EOL);
	      ps->set_property ("page-break-permission", ly_symbol2scm ("allow"));
	      ps->set_property ("page-turn-permission", ly_symbol2scm ("allow"));
	      
	      paper_system_set_stencil (ps, *unsmob_stencil (t));
	      ps->set_property ("is-title", SCM_BOOL_T); 
	      if (scm_is_pair (scm_cdr (list)))
		{
		  /* If an other markup is following, set this markup 
		   * next padding and next space to 0, so that baseline-skip 
		   * only should be taken into account for lines vertical
		   * spacing. */
		  ps->set_property ("next-padding", scm_double2num (0.0));
		  ps->set_property ("next-space", scm_double2num (0.0));
		}
	      system_specs = scm_cons (ps->self_scm (), system_specs);
	      ps->unprotect ();
	      
	      if (scm_is_pair (labels))
		{
		  set_labels (scm_car (system_specs), labels);
		  labels = SCM_EOL;
		}
	      // FIXME: figure out penalty.
	      //set_system_penalty (ps, scores_[i].header_);
	    }
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
  if (scm_is_pair (bookparts_))
    {
      for (SCM p = bookparts_; scm_is_pair (p); p = scm_cdr (p))
	if (Paper_book *pbookpart = unsmob_paper_book (scm_car (p)))
	  systems_ = scm_append_x (scm_list_2 (systems_, pbookpart->systems ()));
    }
  else
    {
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
    }

  return systems_;
}

SCM
Paper_book::pages ()
{
  if (SCM_BOOL_F != pages_)
    return pages_;

  pages_ = SCM_EOL;
  if (scm_is_pair (bookparts_))
    {
      for (SCM p = bookparts_; scm_is_pair (p); p = scm_cdr (p))
	if (Paper_book *pbookpart = unsmob_paper_book (scm_car (p)))
	  pages_ = scm_append_x (scm_list_2 (pages_, pbookpart->pages ()));
    }
  else if (scm_is_pair (scores_))
    {
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
    }
  return pages_;
}

SCM
Paper_book::performances () const
{
  return scm_reverse (performances_);
}
