/*
  input-file-results.cc -- implement some globals

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
#include "ly-module.hh"

bool store_locations_global_b;

/*
  TODO: should merge with My_lily_parser. 
 */

/*
  no ! suffix since it doesn't modify 1st argument.
 */
LY_DEFINE (ly_set_point_and_click, "ly:set-point-and-click", 1, 0, 0,
	  (SCM what),
	  "Set the options for Point-and-click source specials output. The\n"
"argument is a symbol.  Possible options are @code{none} (no source specials),\n"
"@code{line} and @code{line-column}")
{
  /*
    UGH.
   */
  SCM val = SCM_BOOL_F;
  if (ly_symbol2scm ("line-column") == what)
    val = scm_c_eval_string ("line-column-location");
  else if (what == ly_symbol2scm ("line"))
    val = scm_c_eval_string ("line-location");

  scm_module_define (global_lily_module, ly_symbol2scm ("point-and-click"), val);
  store_locations_global_b = is_procedure (val);
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
      if (!dependency_prefix_global.is_empty ())
	{
	  if (dep.index ('/') < 0)
	    dep = dependency_prefix_global + dep;
	}
      out  += " " +  dep;
    }
  fprintf (f, "%s\n",  out.to_str0 ());
}

/* Distill full input file name from command argument.  PATH describes
   file name with added default extension, ".ly" if none.  "-" is
   STDIN.  */
Path
distill_inname (String str)
{
  Path p = split_path (str);
  if (str.is_empty () || str == "-")
    p.base = "-";
  else
    {
      String orig_ext = p.ext;
      char const *extensions[] = {"ly", "", 0};
      for (int i = 0; extensions[i]; i++)
	{
	  p.ext = orig_ext;
	  if (*extensions[i] && !p.ext.is_empty ())
	    p.ext += ".";
	  p.ext += extensions[i];
	  if (!global_path.find (p.to_string ()).is_empty ())
	      break;
	}
      /* Reshuffle extension */
      p = split_path (p.to_string ());
    }
  return p;
}

LY_DEFINE(ly_parse_file, "ly:parse-file",
	  1,0,0,
	  (SCM name),
	  "Parse a single @code{.ly} file. If this fails, then throw @code{ly-file-failed} key. "
	  )
{
  SCM_ASSERT_TYPE (is_string (name), name, SCM_ARG1, __FUNCTION__, "string");
  char const *file = SCM_STRING_CHARS(name);
  
  String infile (file);
  Path inpath = distill_inname (infile);
  
  /* By default, use base name of input file for output file name */
  Path outpath = inpath;
  if (inpath.to_string () != "-")
    outpath.ext = output_format_global;
  
  /* By default, write output to cwd; do not copy directory part
     of input file name */
  outpath.root = "";
  outpath.dir = "";
  
  if (!output_name_global.is_empty ())
    outpath = split_path (output_name_global);
  
  String init;
  if (!init_name_global.is_empty ())
    init = init_name_global;
  else
    init = "init.ly";
  
  String in_file = inpath.to_string ();
  String out_file = outpath.to_string ();


  if (init.length () && global_path.find (init).is_empty ())
    {
      warning (_f ("can't find init file: `%s'", init));
      warning (_f ("(search path: `%s')", global_path.to_string ().to_str0 ()));
      exit (2);
    }

  if ((in_file != "-") && global_path.find (in_file).is_empty ())
    {
      warning (_f ("can't find file: `%s'", in_file));
      scm_throw (ly_symbol2scm ("ly-file-failed"), scm_list_1 (scm_makfrom0str (in_file.to_str0 ())));
    }
  else
    {
      Sources sources;
      sources.set_path (&global_path);
  
      progress_indication (_f ("Now processing `%s'", in_file.to_str0 ()));
      progress_indication ("\n");

      My_lily_parser parser (&sources);
      parser.parse_file (init, in_file, out_file);
  


#if 0
      // fixme dependencies
      if (dependency_global_b)
	{
	  Path p = split_path (output);
	  p.ext = "dep";
	  write_dependency_file (p.to_string (),
				 target_strings_,
				 inclusion_names_);
	}
#endif
  
      if (parser.error_level_)
	{
	  /*
	    TODO: pass renamed input file too.
	  */
	  scm_throw (ly_symbol2scm ("ly-file-failed"), scm_list_1 (scm_makfrom0str (in_file.to_str0 ()))); 
	}
    }
  return SCM_UNSPECIFIED;
}
