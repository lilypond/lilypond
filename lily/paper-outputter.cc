/*
  paper-outputter.cc -- implement Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <time.h>
#include <math.h>

#include "dimensions.hh"
#include "virtual-methods.hh"
#include "paper-outputter.hh"
#include "stencil.hh"
#include "array.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "font-metric.hh"
#include "main.hh"
#include "scm-hash.hh"
#include "lily-version.hh"
#include "paper-def.hh"
#include "input-file-results.hh"
#include "ly-module.hh"
#include "paper-book.hh"
#include "paper-line.hh"
#include "input-smob.hh"  // output_expr


Paper_outputter::Paper_outputter (String name)
{
  if (safe_global_b)
    scm_define (ly_symbol2scm ("safe-mode?"), SCM_BOOL_T);      
  
  file_ = scm_open_file (scm_makfrom0str (name.to_str0 ()),
			 scm_makfrom0str ("w"));

  if (output_format_global == PAGE_LAYOUT)
    {
      output_func_ = SCM_EOL;
      String name = "scm output-" + output_format_global;
      if (safe_global_b)
	{
	  /* In safe mode, start from a GUILE safe-module and import
	     all symbols from the output module.  */
	  scm_c_use_module ("ice-9 safe");
	  SCM msm = scm_primitive_eval (ly_symbol2scm ("make-safe-module"));
 	  output_module_ = scm_call_0 (msm);
	  ly_import_module (output_module_,
			    scm_c_resolve_module (name.to_str0 ()));
	}
      else
	output_module_ = scm_c_resolve_module (name.to_str0 ());

      /* FIXME: output-lib should be module, that can be imported.  */
#define IMPORT_LESS 1 // only import the list of IMPORTS
#if IMPORT_LESS
      scm_c_use_module ("lily");
      scm_c_use_module ("ice-9 regex");
      scm_c_use_module ("srfi srfi-13");
#endif
      char const *imports[] = {
	"lilypond-version",          /* from lily */
	"ly:output-def-scope",
	"ly:gulp-file",
	"ly:number->string",
	"ly:ragged-page-breaks",
	"ly:optimal-page-breaks",
	
	"ly:number-pair->string",    /* output-lib.scm */
	"ly:numbers->string",
	"ly:inexact->string",
	
	"assoc-get",
#if IMPORT_LESS	
	"string-index",              /* from srfi srfi-13 */
	"string-join",
	"regexp-substitute/global",  /* from (ice9 regex) */
#endif	
	0,
      };
      
      for (int i = 0; imports[i]; i++)
	{
	  SCM s = ly_symbol2scm (imports[i]);
	  scm_module_define (output_module_, s, scm_primitive_eval (s));
	}
#ifndef IMPORT_LESS  // rather crude, esp for safe-mode let's not
      SCM m = scm_set_current_module (output_module_);
      /* not present in current module*/
      scm_c_use_module ("ice-9 regex");
      scm_c_use_module ("srfi srfi-13");
      /* Need only a few of these, see above
	 scm_c_use_module ("lily"); */
      scm_set_current_module (m);
#endif
    }
  else
    {
      output_func_
	= scm_call_1 (ly_scheme_function ("find-dumper"),
		      scm_makfrom0str (output_format_global.to_str0 ()));
      output_module_ = SCM_EOL;
    }
}

Paper_outputter::~Paper_outputter ()
{
  scm_close_port (file_);
  file_ = SCM_EOL;
}

void
Paper_outputter::output_scheme (SCM scm)
{
  if (output_format_global == PAGE_LAYOUT)
    scm_display (scm_eval (scm, output_module_), file_);
  else
    scm_call_2 (output_func_, scm, file_);
}

void
Paper_outputter::output_metadata (Paper_def *paper, SCM scopes)
{
  SCM fields = SCM_EOL;
  for (int i = dump_header_fieldnames_global.size (); i--; )
    fields
      = scm_cons (ly_symbol2scm (dump_header_fieldnames_global[i].to_str0 ()),
		 fields);
  output_scheme (scm_list_n (ly_symbol2scm ("output-scopes"),
			     paper->self_scm (),
			     ly_quote_scm (scopes),
			     ly_quote_scm (fields),
			     scm_makfrom0str (basename_.to_str0 ()), 
			     SCM_UNDEFINED));
}

void
Paper_outputter::output_header (Paper_def *paper, SCM scopes, int page_count)
{
  String creator = gnu_lilypond_version_string ();
  creator += " (http://lilypond.org)";
  time_t t (time (0));
  String time_stamp = ctime (&t);
  time_stamp = time_stamp.left_string (time_stamp.length () - 1)
    + " " + *tzname;
  output_scheme (scm_list_4 (ly_symbol2scm ("header"),
			     scm_makfrom0str (creator.to_str0 ()),
			     scm_makfrom0str (time_stamp.to_str0 ()),
			     scm_int2num (page_count)));

  output_metadata (paper, scopes);
  output_music_output_def (paper);

  output_scheme (scm_list_1 (ly_symbol2scm ("header-end")));

  /*
    TODO: maybe have Scheme extract the fonts directly from \paper?

    Alternatively, we could simply load the fonts on demand in the
    output, and do away with this define-fonts step.
   */
  output_scheme (scm_list_3 (ly_symbol2scm ("define-fonts"),
			     paper->self_scm (),
			     ly_quote_scm (paper->font_descriptions ())));
}

void
Paper_outputter::output_line (SCM line, Offset *origin, bool is_last)
{
  Paper_line *pl = unsmob_paper_line (line);
  Offset dim = pl->dim ();
  if (dim[Y_AXIS] > 50 CM)
    {
      programming_error ("Improbable system height.");
      dim[Y_AXIS] = 50 CM;
    }

  if (output_format_global != PAGE_LAYOUT)
    output_scheme (scm_list_3 (ly_symbol2scm ("start-system"),
			       scm_make_real (dim[X_AXIS]),
			       scm_make_real (dim[Y_AXIS])));
  else
    {
      output_scheme (scm_list_3 (ly_symbol2scm ("new-start-system"),
				 ly_quote_scm (ly_offset2scm (*origin)),
				 ly_quote_scm (ly_offset2scm (dim))));
      (*origin)[Y_AXIS] += dim[Y_AXIS];
    }

  SCM between = SCM_EOL;
  for (SCM s = pl->stencils (); ly_pair_p (s); s = ly_cdr (s))
    {
      Stencil *stil = unsmob_stencil (ly_car (s));
      if (stil)
	output_expr (stil->get_expr (), Offset (0,0));
      /* Only if !PAGE_LAYOUT */
      else if (ly_caar (s) == ly_symbol2scm ("between-system-string"))
	between = ly_cdar (s);
    }

  if (is_last)
    output_scheme (scm_list_1 (ly_symbol2scm ("stop-last-system")));
  else
    {
      output_scheme (scm_list_1 (ly_symbol2scm ("stop-system")));
      if (output_format_global != PAGE_LAYOUT && between != SCM_EOL)
	output_scheme (between);
    }
}

void
Paper_outputter::output_music_output_def (Music_output_def* odef)
{
  output_scheme (scm_list_n (ly_symbol2scm ("output-paper-def"),
			     odef->self_scm (), SCM_UNDEFINED));
}

/* TODO: replaceme/rewriteme, see output-ps.scm: output-stencil  */
void
Paper_outputter::output_expr (SCM expr, Offset o)
{
  while (1)
    {
      if (!ly_pair_p (expr))
	return;
  
      SCM head =ly_car (expr);
      if (unsmob_input (head))
	{
	  Input * ip = unsmob_input (head);
      
	  output_scheme (scm_list_n (ly_symbol2scm ("define-origin"),
				     scm_makfrom0str (ip->file_string ().to_str0 ()),
				     scm_int2num (ip->line_number ()),
				     scm_int2num (ip->column_number ()),
				     SCM_UNDEFINED));
	  expr = ly_cadr (expr);
	}
      else  if (head ==  ly_symbol2scm ("no-origin"))
	{
	  output_scheme (scm_list_n (head, SCM_UNDEFINED));
	  expr = ly_cadr (expr);
	}
      else if (head == ly_symbol2scm ("translate-stencil"))
	{
	  o += ly_scm2offset (ly_cadr (expr));
	  expr = ly_caddr (expr);
	}
      else if (head == ly_symbol2scm ("combine-stencil"))
	{
	  output_expr (ly_cadr (expr), o);
	  expr = ly_caddr (expr);
	}
      else
	{
	  output_scheme (scm_list_n (ly_symbol2scm ("placebox"),
				     scm_make_real (o[X_AXIS]),
				     scm_make_real (o[Y_AXIS]),
				     expr,
				     SCM_UNDEFINED));
	  return;
	}
    }
}

