/*
  scores.cc -- implement some globals

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "config.h"

#include <errno.h>
#include <sys/types.h>
#include <stdio.h>


#if HAVE_SYS_STAT_H 
#include <sys/stat.h>
#endif
#include <unistd.h>


#include "main.hh"
#include "score.hh"
#include "string.hh"
#include "paper-def.hh"
#include "warn.hh"
#include "parray.hh"
#include "file-path.hh"
#include "input-file-results.hh"
#include "my-lily-parser.hh"
#include "source.hh"
#include "lily-version.hh"
#include "scm-hash.hh"


LY_DEFINE(ly_set_point_and_click_x, "set-point-and-click!", 1, 0, 0,
	  (SCM what),
	  "Set the options for Point-and-click source specials output. The
argument is a symbol.  Possible options are @code{none} (no source specials),
@code{line} and @code{line-column}")
{
  /*
    UGH.
   */
  SCM val = SCM_BOOL_F;
  if (ly_symbol2scm ("line-column") == what)
    val = gh_eval_str ("line-column-location");
  else if (what == ly_symbol2scm ("line"))
    val = gh_eval_str ("line-location");

  extern SCM lily_module; 
  scm_module_define (lily_module, ly_symbol2scm ("point-and-click"), val);
  return SCM_UNSPECIFIED;
}

void
write_dependency_file (String fn,
		       Array<String> targets,
		       Array<String> deps)
{
  const int WRAPWIDTH = 65;

  progress_indication (_f ("dependencies output to `%s'...", fn.to_str0 ()));
  progress_indication ("\n");
  FILE * f = fopen  (fn.to_str0 (), "w");
  if (!f)
    warning (_f ("can't open file: `%s'", fn));

  fprintf (f, "# Generated automatically by: %s\n", gnu_lilypond_version_string ().to_str0 ());
  
  String out;
  for (int i=0; i < targets.size (); i ++)
     out += dependency_prefix_global + targets[i] + " ";
  out +=  ": ";

  for (int i=0; i < deps.size (); i ++)
    {
      if (out.length () > WRAPWIDTH)
	{
	  fprintf (f, "%s\\\n", out.to_str0 ());
	  out = "  ";
	}
      String dep = deps[i];
      if (!dependency_prefix_global.empty_b ())
	{
#if 0//thinko?
	  if (stat (dep.to_str0 (), &stat_buf) == -1 && errno == ENOENT)
	    ; //make emacs happy
#else
	  if (dep.index ('/') < 0)
#endif
	    dep = dependency_prefix_global + dep;
	}
      out  += " " +  dep;
    }
  fprintf (f, "%s\n",  out.to_str0 ());
}


void
Input_file_results::do_deps ()
{
  if (dependency_global_b)
    {
      Path p = split_path (output_name_global);
      p.ext = "dep";
      write_dependency_file (p.string (),
			     target_strings_,
			     inclusion_names_);
    }
}


void
Input_file_results::do_scores ()
{
  if (header_ == SCM_EOL)
    header_ = ly_make_anonymous_module ();

  for (int i=0; i < scores_.size (); i++)
    {
      Score* is = scores_[i];
      is->input_file_ = this;
      
      if (is->errorlevel_)
	{
	  is->warning (_ ("Score contains errors; will not process it"));
	  exit_status_global |= 1;
	}
      else
	{
	  is->process ();
	}
    }
  do_deps ();
}

Input_file_results::~Input_file_results ()
{
  for (int i=0; i < scores_.size (); i++)
    scm_gc_unprotect_object (scores_[i]->self_scm ());
  scores_.clear ();
  
  inclusion_names_.clear ();
  if (header_)
    header_ = SCM_EOL;

  global_input_file =0;
}


Input_file_results* global_input_file;

Input_file_results::Input_file_results (String init_string, String file_string)
{
  header_ = SCM_EOL;
  global_input_file = this;
  ly_set_point_and_click_x (SCM_BOOL_F);
  
  sources_.set_path (&global_path);
  
  My_lily_parser parser (this);

  progress_indication (_f ("Now processing: `%s'", file_string.to_str0 ()));
  progress_indication ("\n");
  parser.parse_file (init_string, file_string);
  
  if (parser.error_level_)
    {
      exit_status_global  = 1;
    }
  else
    do_scores ();
  
}


void
do_one_file (String init_string, String file_string) 
{
   if (init_string.length () && global_path.find (init_string).empty_b ())
    {
      warning (_f ("can't find file: `%s'", init_string));
      warning (_f ("(search path: `%s')", global_path.string ().to_str0 ()));
      return;
    }
  if ((file_string != "-") && global_path.find (file_string).empty_b ())
    {
      warning (_f ("can't find file: `%s'", file_string));
      return;
    }

  Input_file_results inp_file(init_string, file_string);
}
