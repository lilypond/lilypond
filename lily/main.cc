/*
  main.cc -- implement main: entrypoints

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <locale.h>
#include <string.h>

#include "config.h"

#if HAVE_GETTEXT
#include <libintl.h>
#endif

#include "lily-guile.hh"
#include "lily-version.hh"
#include "all-font-metrics.hh"
#include "getopt-long.hh"
#include "misc.hh"
#include "string.hh"
#include "main.hh"
#include "file-path.hh"
#include "file-results.hh"
#include "warn.hh"
#include "lily-guile.hh"
#include "paper-def.hh"
#include "midi-def.hh"
#include "global-ctor.hh"
#include "kpath.hh"

static int sane_putenv (char const* key, char const* value, bool overwrite = false);

/*
  Global options that can be overridden through command line.
*/

/* Write dependencies file? */
bool dependency_global_b = false;

/* Prepend to dependencies */
String dependency_prefix_global;

/* Names of header fields to be dumped to a separate file. */
Array<String> dump_header_fieldnames_global;

/* Name of initialisation file. */
String init_name_global;

/* Do not calculate and write paper output? */
bool no_paper_global_b = false;

/* Selected output format.
   One of tex, ps, scm, as. */
String output_format_global = "tex";

/* Current output name. */
String output_name_global;

/* Run in safe mode? -- FIXME: should be re-analised */
bool safe_global_b = false;

/* Verbose progress indication? */
bool verbose_global_b = false;

/* Scheme code to execute before parsing, after .scm init */
String init_scheme_code_string = "(begin #t ";


/*
  Misc. global stuff.
 */


All_font_metrics *all_fonts_global;
int exit_status_global;
File_path global_path;

/* Number of current score output block.  If there's more than one
   score block, this counter will be added to the output filename. */
int score_count_global;



/*
  File globals.
 */

/*  The option parser */
static Getopt_long *oparser_p_static = 0;

/*
 Internationalisation kludge in two steps:
   * use _i () to get entry in POT file
   * call gettext () explicitely for actual "translation"

 Note: these messages all start with lower case (ie, don't
       follow regular localisation guidelines).
 */
static Long_option_init options_static[] = {
  /* print example usage:  lilypond -e "" ? */
  {_i ("EXPR"), "evaluate", 'e',_i ("Scheme options: try -e \"(set-lily-option 'help 0)\" for more help.")},
  /* another bug in option parser: --output=foe is taken as an abbreviation
     for --output-format */
  {_i ("EXT"), "format", 'f',  _i ("use output format EXT")},
  {0, "help", 'h',  _i ("this help")},
  {_i ("FIELD"), "header", 'H',  _i ("write header field to BASENAME.FIELD")},
  {_i ("DIR"), "include", 'I',  _i ("add DIR to search path")},
  {_i ("FILE"), "init", 'i',  _i ("use FILE as init file")},
  {0, "dependencies", 'M',  _i ("write Makefile dependencies for every input file")},
  {0, "no-paper", 'm',  _i ("produce MIDI output only")},
  {_i ("FILE"), "output", 'o',  _i ("write output to FILE")},
  {_i ("DIR"), "dep-prefix", 'P',  _i ("prepend DIR to dependencies")},
#if 0
  /*
    should audit again.
   */
  {0, "safe", 's',  _i ("inhibit file output naming and exporting")},
#endif
  {0, "version", 'v',  _i ("print version number")},
  {0, "verbose", 'V', _i ("verbose")},
  {0, "warranty", 'w',  _i ("show warranty and copyright")},
  {0,0,0,0}
};

void
identify (FILE *out)
{
  fputs (gnu_lilypond_version_string ().to_str0 (), out);
}

void
dirinfo (FILE *out)
{
  fputs ("\n", out);
  fprintf (out, "lilypond_datadir: `%s'\n", LILYPOND_DATADIR);
  fprintf (out, "local_lilypond_datadir: `%s'\n", LOCAL_LILYPOND_DATADIR);
  fprintf (out, "localedir: `%s'\n", LOCALEDIR);

  char *lilypond_prefix = getenv ("LILYPONDPREFIX");
  fprintf (out, "LILYPONDPREFIX: `%s'\n",
	   (lilypond_prefix ? lilypond_prefix : ""));
}

void
usage ()
{
  printf ("\n");
  /* No version number or newline here. It confuses help2man.  */
  printf (_f ("Usage: %s [OPTION]... FILE...", "lilypond").to_str0 ());
  printf ("\n\n");
  printf (_ ("Typeset music and or play MIDI from FILE").to_str0 ());
  printf ("\n\n");
  printf(_ (
"LilyPond is a music typesetter.  It produces beautiful sheet music\n"
"using a high level description file as input.  LilyPond is part of \n"
"the GNU Project.\n"
).to_str0 ());

  printf ("\n");
  printf (_ ("Options:").to_str0 ());
  printf ("\n");
  printf (Long_option_init::table_string (options_static).to_str0 ());
  printf ("\n");
  printf ("\n");

  printf (_f ("Report bugs to %s", "bug-lilypond@gnu.org").to_str0 ());
  printf ("\n");
}

void
version ()
{
  identify (stdout);
  printf ("\n");
  printf (_f (
  "This is free software.  It is covered by the GNU General Public License,\n"
  "and you are welcome to change it and/or distribute copies of it under\n"
  "certain conditions.  Invoke as `%s --warranty' for more information.\n",
    "lilypond").to_str0 ());
  printf ("\n");

  printf (_f ("Copyright (c) %s by", "1996--2002").to_str0 ());
  printf ("\n");
  printf ("  Han-Wen Nienhuys <hanwen@cs.uu.nl>\n");
  printf ("  Jan Nieuwenhuizen <janneke@gnu.org>\n");
}

void
notice ()
{
  printf ("\n");
  printf (_ ("GNU LilyPond -- The music typesetter").to_str0 ());
  printf ("\n");
  printf (_f ("Copyright (c) %s by", "1996--2002").to_str0 ());
  printf ("\n");
  printf ("  Han-Wen Nienhuys <hanwen@cs.uu.nl>\n");
  printf ("  Jan Nieuwenhuizen <janneke@gnu.org>\n");
  printf ("\n");
  printf ( _ (
	     "    This program is free software; you can redistribute it and/or\n"
	     "modify it under the terms of the GNU General Public License version 2\n"
	     "as published by the Free Software Foundation.\n"
	     "\n"
	     "    This program is distributed in the hope that it will be useful,\n"
	     "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
	     "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n"
	     "General Public License for more details.\n"
	     "\n"
	     "    You should have received a copy (refer to the file COPYING) of the\n"
	     "GNU General Public License along with this program; if not, write to\n"
	     "the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,\n"
	     "USA.\n").to_str0 ());
}


/* Where LilyPond's init files live.  Typically:
   LILYPOND_DATADIR = /usr/local/share/lilypond
   LOCAL_LILYPOND_DATADIR = /usr/local/share/lilypond/1.5.68 */
char const *prefix_directory[2] = {LILYPOND_DATADIR, LOCAL_LILYPOND_DATADIR};

void
setup_paths ()
{
  if (char const *lilypond_prefix = getenv ("LILYPONDPREFIX"))
    prefix_directory[1] = lilypond_prefix;

#if HAVE_GETTEXT
  setlocale (LC_ALL, ""); /* enable locales */
  setlocale (LC_NUMERIC, "C"); /* musn't have comma's in TeX output... */
  String lily_locale_dir;
  String name (PACKAGE);
  name.to_lower ();

  /*
    urg; what *do* we want with $LILYPONDPREFIX, DIR_DATADIR and $prefix/share
    handy for multiple source-dir runs, though...
   */
  if (!String (prefix_directory[0]).empty_b ())
    {
      lily_locale_dir = String (prefix_directory[0]) + "/share/locale";
      bindtextdomain (name.to_str0 (), lily_locale_dir.to_str0 ());
    }
  else
    bindtextdomain (name.to_str0 (), LOCALEDIR);
  textdomain (name.to_str0 ());
#endif

  global_path.add ("");


  /* Adding mf/out make lilypond unchanged source directory, when setting
     LILYPONDPREFIX to lilypond-x.y.z */
  char *suffixes[] = {"ly", "afm", "mf/out", "scm", "tfm", "ps", 0};

  for (unsigned i = 0; i < sizeof (prefix_directory)
	 / sizeof (*prefix_directory); i++)
    for (char **s = suffixes; *s; s++)
      {
	String p = prefix_directory[i] + to_string ('/') + String (*s);
	global_path.add (p);
	
#if !KPATHSEA
	/* Urg: GNU make's $ (word) index starts at 1 */
	int i  = 1;
	while (global_path.try_add (p + to_string (".") + to_string (i)))
	  i++;
#endif
      }
  }
  
/**
  Make input file name from command argument.

  Path describes file name with added default extension,
  ".ly" if none.  "-" is stdin.
 */
Path
distill_inname (String str)
{
  Path p = split_path (str);
  if (str.empty_b () || str == "-")
    p.base = "-";
  else
    {
      String orig_ext = p.ext;
      char const *extensions[] = {"ly", "fly", "sly", "", 0};
      for (int i = 0; extensions[i]; i++)
	{
	  p.ext = orig_ext;
	  if (*extensions[i] && !p.ext.empty_b ())
	    p.ext += ".";
	  p.ext += extensions[i];
	  if (!global_path.find (p.string ()).empty_b ())
	      break;
	}
      /* Reshuffle extension */
      p = split_path (p.string ());
    }
  return p;
}

String
format_to_ext (String format)
{
  if (format == "tex")
    /* .lytex change put off */
    return "tex"; // "lytex";
  return format;
}

void
prepend_load_path (String dir)
{
  String s = "(set! %load-path (cons \""
    + dir
    + "\" %load-path))";
  scm_c_eval_string (s.to_str0 ());
}

void
main_prog (void *, int, char **)
{
  /* Engravers use lily.scm contents, need to make Guile find it.
     Prepend onto GUILE %load-path, very ugh. */
     
  for (unsigned i = 0; i < sizeof (prefix_directory)
	 / sizeof (*prefix_directory); i++)
    {
      prepend_load_path (prefix_directory[i]);
      /* Junk this.  We should make real modules iso. just loading files. */
      prepend_load_path (String (prefix_directory[i]) + "/scm");
    }

  if (verbose_global_b)
    dirinfo (stderr);
  
  ly_init_guile ();

  printf ("\n");

  call_constructors ();
  all_fonts_global = new All_font_metrics (global_path.string ());

  init_scheme_code_string += ")";
  gh_eval_str ((char *)init_scheme_code_string.to_str0 ());
  
  int p=0;
  const char *arg  = oparser_p_static->get_next_arg ();
  
  do
    {
      String infile (arg);
      Midi_def::reset_score_count ();
      Paper_def::reset_score_count ();

      Path inpath = distill_inname (infile);

      /* By default, use base name of input file for output file name */
      Path outpath = inpath;
      if (inpath.string () != "-")
	outpath.ext = format_to_ext (output_format_global);

      /* By default, write output to cwd; do not copy directory part
         of input file name */
      outpath.root = "";
      outpath.dir = "";
      
      if (!output_name_global.empty_b ())
	outpath = split_path (output_name_global);
      
      String init;
      if (!init_name_global.empty_b ())
	init = init_name_global;
      else if (!inpath.ext.empty_b ())
	init = "init." + inpath.ext;
      else
	init = "init.ly";
	
      /* Burp: output name communication goes through _global */
      String save_output_name_global = output_name_global;
      output_name_global = outpath.string ();
      do_one_file (init, inpath.string ());
      output_name_global = save_output_name_global;
      
      p++;
    } while ((arg  = oparser_p_static->get_next_arg ()));
  delete oparser_p_static;
  oparser_p_static = 0;
  exit (exit_status_global);
}


static int
sane_putenv (char const* key, char const* value, bool overwrite)
{
  if (overwrite || !getenv (key))
    {
      String combine = String (key) + "=" + String (value);
      char * s = strdup(combine.to_str0 ());
      return putenv (s);
    }
  return -1;
}

int
main (int argc, char **argv)
{
  setup_paths ();

  /*
    
    These settings hopefully prepare lily for a lot of memory usage.

    In practice the effect on GC times is barely measurable -- larger
    segments cost slighly less time for the conservative marking. (but
    not impressively much)
    
  */
  sane_putenv ("GUILE_INIT_SEGMENT_SIZE_1", "4194304", false);
  sane_putenv ("GUILE_MAX_SEGMENT_SIZE", "8388608", false);

  ly_init_kpath (argv[0]);

  bool help_b = false;
  oparser_p_static = new Getopt_long (argc, argv, options_static);
  while (Long_option_init const * opt = (*oparser_p_static) ())
    {
      switch (opt->shortname_char_)
	{
	case 'v':
	  version ();
	  exit (0);		// we print a version anyway.
	  break;
	case 'o':
	  {
	    String s = oparser_p_static->optional_argument_str0_;
	    Path p = split_path (s);
	    if (s != "-" && p.ext.empty_b ())
	      p.ext = format_to_ext (output_format_global);
	    output_name_global = p.string ();
	  }
	  break;
	case 'e':
	  init_scheme_code_string +=
	    oparser_p_static->optional_argument_str0_;
	  break;
	case 'w':
	  notice ();
	  exit (0);
	  break;
	case 'f':
	  if (oparser_p_static->optional_argument_str0_ == "help")
	    {
	      printf("See http://lilypond.org/wiki?OutputFormats for more information.\n"\
		     "This option is for developers only.\n");
	      exit (0);
	    }
	  output_format_global = oparser_p_static->optional_argument_str0_;
	  break;
	case 'P':
	    dependency_prefix_global = oparser_p_static->optional_argument_str0_;
	  break;
	case 'H':
	  dump_header_fieldnames_global.push (oparser_p_static->optional_argument_str0_);
	  break;
	case 'I':
	  global_path.push (oparser_p_static->optional_argument_str0_);
	  break;
	case 'i':
	  init_name_global = oparser_p_static->optional_argument_str0_;
	  break;
	case 'h':
	  help_b = true;
	  break;
	case 'V':
	  verbose_global_b = true;
	  break;
	case 's':
	  safe_global_b = true;
	  break;
	case 'M':
	  dependency_global_b = true;
	  break; 
	case 'm':
	  no_paper_global_b = true;
	  break;
	default:
	  assert (false);
	  break;
	}
    }
  identify (stderr);

  if (help_b)
    {
      usage ();
      if (verbose_global_b)
	dirinfo (stdout);
      exit (0);
    }


  if (!oparser_p_static->current_arg () )
    {
      usage ();
      /* No FILE arguments is now a usage error */
      exit (2);
    }

  scm_boot_guile (argc, argv, (void (*) (void*, int, char**))main_prog, 0);

  return 0;			// unreachable
}
