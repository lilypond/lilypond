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
#include "input-smob.hh"  // output_expr


Paper_outputter::Paper_outputter (String name)
{
  if (safe_global_b)
    scm_define (ly_symbol2scm ("safe-mode?"), SCM_BOOL_T);      
  
  file_ = scm_open_file (scm_makfrom0str (name.to_str0 ()),
			 scm_makfrom0str ("w"));

  static SCM find_dumper;
  if (!find_dumper)
    find_dumper = scm_c_eval_string ("find-dumper");

  output_func_
    = scm_call_1 (find_dumper,
		  scm_makfrom0str (output_format_global.to_str0 ()));

  String creator = gnu_lilypond_version_string ();
  creator += " (http://lilypond.org)";
  time_t t (time (0));
  String time_stamp = ctime (&t);
  time_stamp = time_stamp.left_string (time_stamp.length () - 1)
    + " " + *tzname;
  output_scheme (scm_list_3 (ly_symbol2scm ("header"),
			     scm_makfrom0str (creator.to_str0 ()),
			     scm_makfrom0str (time_stamp.to_str0 ())));
}

Paper_outputter::~Paper_outputter ()
{
  scm_close_port (file_);
  file_ = SCM_EOL;
}

void
Paper_outputter::output_scheme (SCM scm)
{
  gh_call2 (output_func_, scm, file_);
}

void
Paper_outputter::output_metadata (SCM scopes, Paper_def *paper)
{
  SCM fields = SCM_EOL;
  for (int i = dump_header_fieldnames_global.size (); i--; )
    fields
      = gh_cons (ly_symbol2scm (dump_header_fieldnames_global[i].to_str0 ()),
		 fields);
  output_scheme (scm_list_n (ly_symbol2scm ("output-scopes"),
			     paper->self_scm (),
			     scm_list_n (ly_symbol2scm ("quote"),
					 scopes, SCM_UNDEFINED),
			     scm_list_n (ly_symbol2scm ("quote"),
					 fields, SCM_UNDEFINED),
			     scm_makfrom0str (basename_.to_str0 ()), 
			     SCM_UNDEFINED));
}

void
Paper_outputter::output_header (Paper_def *paper)
{
  output_music_output_def (paper);
  output_scheme (scm_list_1 (ly_symbol2scm ("header-end")));
  output_scheme (scm_list_2 (ly_symbol2scm ("define-fonts"),
			     ly_quote_scm (paper->font_descriptions ())));
}

void
Paper_outputter::output_line (SCM line, bool is_last)
{
  Offset dim = ly_scm2offset (ly_car (line));
  Real width = dim[X_AXIS];
  Real height = dim[Y_AXIS];
      
  if (height > 50 CM)
    {
      programming_error ("Improbable system height.");
      height = 50 CM;
    }

  output_scheme (scm_list_3 (ly_symbol2scm ("start-system"),
			     gh_double2scm (width), gh_double2scm (height)));

  SCM between = SCM_EOL;
  for (SCM s = ly_cdr (line); gh_pair_p (s); s = ly_cdr (s))
    {
      Stencil *stil = unsmob_stencil (ly_cdar (s));
      SCM head = ly_caar (s);
      
      if (head == ly_symbol2scm ("between-system-string"))
	{
	  between = stil->get_expr ();
	  continue;
	}

      if (stil)
	output_expr (stil->get_expr (), ly_scm2offset (head));
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
      if (!gh_pair_p (expr))
	return;
  
      SCM head =ly_car (expr);
      if (unsmob_input (head))
	{
	  Input * ip = unsmob_input (head);
      
	  output_scheme (scm_list_n (ly_symbol2scm ("define-origin"),
				     scm_makfrom0str (ip->file_string ().to_str0 ()),
				     gh_int2scm (ip->line_number ()),
				     gh_int2scm (ip->column_number ()),
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
				     gh_double2scm (o[X_AXIS]),
				     gh_double2scm (o[Y_AXIS]),
				     expr,
				     SCM_UNDEFINED));
	  return;
	}
    }
}

