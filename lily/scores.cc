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
#include "file-results.hh"
#include "my-lily-parser.hh"
#include "source.hh"
#include "lily-version.hh"
#include "scm-hash.hh"

Sources* source_global = 0;
Array<String> inclusion_globals;
Array<String> target_string_globals;
Link_array<Score> score_globals;
Scheme_hash_table * global_header;


void write_dependency_file (String fn, Array<String> targets,
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
do_deps ()
{
  if (dependency_global_b)
    {
      Path p = split_path (output_name_global);
      p.ext = "dep";
      write_dependency_file (p.string (),
			     target_string_globals,
			     inclusion_globals);
    }
}


void
do_scores ()
{
  if (!global_header)
    global_header = new Scheme_hash_table;
  for (int i=0; i < score_globals.size (); i++)
    {
      Score* is = score_globals[i];

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

void
clear_scores ()
{
  for (int i=0; i < score_globals.size (); i++)
    scm_gc_unprotect_object (score_globals[i]->self_scm ());
  score_globals.clear ();
  
  inclusion_globals.clear ();
  if (global_header)
    scm_gc_unprotect_object (global_header ->self_scm ());
  global_header =0; 
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

  Sources sources;
  source_global = &sources;
  source_global->set_path (&global_path);
  {
    My_lily_parser parser (source_global);
    parser.set_version_check (false);
    progress_indication (_f ("Now processing: `%s'", file_string.to_str0 ()));
    progress_indication ("\n");
    parser.parse_file (init_string, file_string);

    if (parser.error_level_)
      {
	exit_status_global  = 1;
      }
    else
      do_scores ();
    clear_scores ();
  }
  source_global = 0;
}

