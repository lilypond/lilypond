/*
  main.cc -- implement main () entrypoint.

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <locale.h>
#include <string.h>

#include "config.hh"

#if HAVE_GETTEXT
#include <libintl.h>
#endif

#include "all-font-metrics.hh"
#include "file-name.hh"
#include "file-path.hh"
#include "getopt-long.hh"
#include "global-ctor.hh"
#include "kpath.hh"
#include "lily-guile.hh"
#include "lily-version.hh"
#include "main.hh"
#include "misc.hh"
#include "output-def.hh"
#include "string.hh"
#include "warn.hh"

/*
 * Global options that can be overridden through command line.
 */

/* Names of header fields to be dumped to a separate file. */
Array<String> dump_header_fieldnames_global;

/* Name of initialisation file. */
String init_name_global;

/* Do not calculate and write layout output? */
bool no_layout_global_b = false;

/* Selected output format.
   One of tex, ps, scm, as. */
String output_format_global = "tex";

/* Current output name. */
String output_name_global;

/* Run in safe mode? */
bool safe_global_b = false;

/* Verbose progress indication? */
bool verbose_global_b = false;

/* Scheme code to execute before parsing, after .scm init
   This is where -e arguments are appended to.
*/
String init_scheme_code_string = "(begin #t ";

bool make_pdf = false;
bool make_dvi = false;
bool make_ps = false;
bool make_png = false;
bool make_preview = false;
bool make_pages = true;
bool make_tex = false;

/*
 * Miscellaneous global stuff.
 */
File_path global_path;


/*
 * File globals.
 */

static char const *AUTHORS =
"  Han-Wen Nienhuys <hanwen@cs.uu.nl>\n"
"  Jan Nieuwenhuizen <janneke@gnu.org>\n";

static char const *PROGRAM_NAME = "lilypond";
static char const *PROGRAM_URL = "http://lilypond.org";

static char const *NOTICE =
_i ("This program is free software.  It is covered by the GNU General Public\n"
    "License and you are welcome to change it and/or distribute copies of it\n"
    "under certain conditions.  Invoke as `lilypond --warranty' for more\n"
    "information.\n");
  
static char const *WARRANTY =
_i ("    This program is free software; you can redistribute it and/or\n"
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
    "the Free Software Foundation, Inc., 59 Temple Place - Suite 330,\n"
    "Boston, MA 02111-1307, USA.\n");


/* Where the init files live.  Typically:
   LILYPOND_DATADIR = /usr/share/lilypond
   LOCAL_LILYPOND_DATADIR = /usr/share/lilypond/<VERSION> */
char const *prefix_directory[] = {LILYPOND_DATADIR, LOCAL_LILYPOND_DATADIR, 0};

/*  The option parser */
static Getopt_long *option_parser = 0;

/* Internationalisation kludge in two steps:
   * use _i () to get entry in POT file
   * call gettext () explicitely for actual "translation"  */

static Long_option_init options_static[] =
  {
    {_i ("EXPR"), "evaluate", 'e',
     _i ("set option, use -e '(ly:option-usage)' for help")},
    /* Bug in option parser: --output=foe is taken as an abbreviation
       for --output-format.  */
    {_i ("EXT"), "format", 'f', _i ("select back-end to use")},
    {0, "help", 'h',  _i ("print this help")},
    {_i ("FIELD"), "header", 'H',  _i ("write header field to BASENAME.FIELD")},
    {_i ("DIR"), "include", 'I',  _i ("add DIR to search path")},
    {_i ("FILE"), "init", 'i',  _i ("use FILE as init file")},
    {0, "no-layout", 'm',  _i ("produce MIDI output only")},
    {_i ("FILE"), "output", 'o',  _i ("write output to FILE")},
    {0, "preview", 'p',  _i ("generate a preview")},
    {0, "no-pages", 0,  _i ("don't generate full pages")},
    {0, "png", 0,  _i ("generate PNG")},
    {0, "ps", 0,  _i ("generate PostScript")},
    {0, "dvi", 0,  _i ("generate DVI")},
    {0, "pdf", 0,  _i ("generate PDF (default)")},
    {0, "tex", 0,  _i ("generate TeX")},
    {0, "safe-mode", 's',  _i ("run in safe mode")},
    {0, "version", 'v',  _i ("print version number")},
    {0, "verbose", 'V', _i ("be verbose")},
    {0, "warranty", 'w',  _i ("show warranty and copyright")},
    {0,0,0,0}
  };

static void
dir_info (FILE *out)
{
  fputs ("\n", out);
  fprintf (out, "LILYPOND_DATADIR=\"%s\"\n", LILYPOND_DATADIR);
  fprintf (out, "LOCAL_LILYPOND_DATADIR=\"\%s\"\n", LOCAL_LILYPOND_DATADIR);
  fprintf (out, "LOCALEDIR=\"%s\"\n", LOCALEDIR);

  char *lilypond_prefix = getenv ("LILYPONDPREFIX");
  fprintf (out, "LILYPONDPREFIX=\"%s\"\n",
	   (lilypond_prefix ? lilypond_prefix : ""));
}

static void
copyright ()
{
  printf (_f ("Copyright (c) %s by\n%s  and others.",
	      "1996--2004",
	      AUTHORS).to_str0 ());
  printf ("\n");
}

static void
identify (FILE *out)
{
  fputs (gnu_lilypond_version_string ().to_str0 (), out);
  fputs ("\n", out);
}
 
static void
notice ()
{
  identify (stdout);
  printf (_f (NOTICE, PROGRAM_NAME).to_str0 ());
  printf ("\n");
  copyright ();
}

static void
usage ()
{
  /* No version number or newline here.  It confuses help2man.  */
  printf (_f ("Usage: %s [OPTION]... FILE...", PROGRAM_NAME).to_str0 ());
  printf ("\n\n");
  printf (_ ("Typeset music and/or produce MIDI from FILE.").to_str0 ());
  printf ("\n\n");
  printf (_ ("LilyPond produces beautiful music notation.").to_str0 ());
  printf ("\n");
  printf (_f ("For more information, see %s", PROGRAM_URL).to_str0 ());
  printf ("\n\n");
  printf (_ ("Options:").to_str0 ());
  printf ("\n");
  printf (Long_option_init::table_string (options_static).to_str0 ());
  printf ("\n");
  printf (_f ("Report bugs to %s.", "bug-lilypond@gnu.org").to_str0 ());
  printf ("\n");
  printf ("\n");
}

static void
warranty ()
{
  identify (stdout);
  printf ("\n");
  copyright ();
  printf ("\n");
  printf (_ (WARRANTY).to_str0 ());
}

static void
setup_paths ()
{
  if (char const *lilypond_prefix = getenv ("LILYPONDPREFIX"))
    prefix_directory[1] = lilypond_prefix;

  global_path.append ("");

  /* Adding mf/out make lilypond unchanged source directory, when setting
     LILYPONDPREFIX to lilypond-x.y.z */
  char *suffixes[] = {"ly", "afm", "mf/out", "scm", "tfm", "ps", 0};

  for (unsigned i = 0; prefix_directory[i]; i++)
    for (char **s = suffixes; *s; s++)
      {
	String p = prefix_directory[i] + to_string ('/') + String (*s);
	global_path.prepend (p);
	
#if !KPATHSEA
	/* Urg: GNU make's $ (word) index starts at 1 */
	int i  = 1;
	while (global_path.try_append (p + to_string (".") + to_string (i)))
	  i++;
#endif
      }
}
  
static void
prepend_load_path (String dir)
{
  String s = "(set! %load-path (cons \"" + dir + "\" %load-path))";
  scm_c_eval_string (s.to_str0 ());
}

static void
determine_output_options ()
{
  
  bool found_tex = false;
  SCM formats = ly_output_formats ();
  for (SCM s = formats; scm_is_pair (s); s = scm_cdr (s)) 
    {
      found_tex = found_tex || (ly_scm2string (scm_car (s)) == "tex");
    }

      
  if (make_pdf || make_png)
    {
      make_ps = true;
    }
  if (make_ps && found_tex)
    {
      make_dvi = true;
    }
  if (make_dvi && found_tex)
    {
      make_tex = true;
    }
  if (!(make_dvi
	|| make_tex
	|| make_ps
	|| make_png
	|| make_pdf))
    {
      make_pdf = true;
      make_ps = true;
      if (found_tex)
	{
	  make_dvi = true;
	  make_tex = true;
	}
    }
}

static void
main_with_guile (void *, int, char **)
{
  /* Engravers use lily.scm contents, need to make Guile find it.
     Prepend onto GUILE %load-path, very ugh. */
  for (unsigned i = 0; prefix_directory[i]; i++)
    {
      prepend_load_path (prefix_directory[i]);
      /* Junk this.  We should make real modules iso. just loading files. */
      prepend_load_path (String (prefix_directory[i]) + "/scm");
    }

  if (verbose_global_b)
    dir_info (stderr);

  ly_c_init_guile ();
  call_constructors ();

  determine_output_options ();  
  all_fonts_global = new All_font_metrics (global_path.to_string ());

  init_scheme_code_string += ")";
  scm_c_eval_string ((char*) init_scheme_code_string.to_str0 ());

  /* We accept multiple independent music files on the command line to
     reduce compile time when processing lots of small files.
     Starting the GUILE engine is very time consuming.  */

  SCM files = SCM_EOL;
  SCM *tail = &files;
  while (char const *arg = option_parser->get_next_arg ())
    {
      *tail = scm_cons (scm_makfrom0str (arg), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }
  delete option_parser;
  option_parser = 0;

  if (files == SCM_EOL)
    {
      /* No FILE arguments is now a usage error to help newbies.  If you
	 want a filter, you're not a newbie and should know to use file
	 argument `-'.  */
      usage ();
      exit (2);
    }

  SCM result = scm_call_1 (ly_scheme_function ("lilypond-main"), files);
  (void) result;


  /* Unreachable.  */
  exit (0);
}

static void
setup_localisation ()
{
#if HAVE_GETTEXT
  /* Enable locales */
  setlocale (LC_ALL, "");
  
  /* FIXME: check if this is still true.
    Disable localisation of float values.  This breaks TeX output.  */
  setlocale (LC_NUMERIC, "C");
  
  String name (PACKAGE);
  name.to_lower ();
  bindtextdomain (name.to_str0 (), LOCALEDIR);
  textdomain (name.to_str0 ());
#endif
}

static void
parse_argv (int argc, char **argv)
{
  bool help_b = false;
  option_parser = new Getopt_long (argc, argv, options_static);
  while (Long_option_init const *opt = (*option_parser) ())
    {
      switch (opt->shortname_char_)
	{
	case 0:
	  if (String (opt->longname_str0_) == "png")
	    make_png = true;
	  else if (String (opt->longname_str0_) == "pdf")
	    make_pdf = true;
	  else if (String (opt->longname_str0_) == "ps")
	    make_ps = true;
	  else if (String (opt->longname_str0_) == "dvi")
	    make_dvi = true;
	  else if (String (opt->longname_str0_) == "tex")
	    make_tex = true;
	  else if (String (opt->longname_str0_) == "preview")
	    make_preview = true;
	  else if (String (opt->longname_str0_) == "no-pages")
	    make_pages = false;
	  break;
	  
	case 'v':
	  notice ();
	  exit (0);
	  break;
	case 'o':
	  {
	    String s = option_parser->optional_argument_str0_;
	    File_name file_name (s);
	    output_name_global = file_name.to_string ();
	  }
	  break;
	case 'e':
	  init_scheme_code_string += option_parser->optional_argument_str0_;
	  break;
	case 'w':
	  warranty ();
	  exit (0);
	  break;
	case 'f':
	  if (option_parser->optional_argument_str0_ == "help")
	    {
	      printf (_ ("This option is for developers only.").to_str0 ());
	      printf (_ ("Read the sources for more information.").to_str0 ());
	      exit (0);
	    }
	  output_format_global = option_parser->optional_argument_str0_;
	  break;
	case 'H':
	  dump_header_fieldnames_global
	    .push (option_parser->optional_argument_str0_);
	  break;
	case 'I':
	  global_path.append (option_parser->optional_argument_str0_);
	  break;
	case 'i':
	  init_name_global = option_parser->optional_argument_str0_;
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
	case 'm':
	  no_layout_global_b = true;
	  break;
	case 'p':
	  make_preview = true;
	  break;
	default:
	  programming_error (to_string ("unhandled short option: %c",
					opt->shortname_char_));
	  assert (false);
	  break;
	}
    }

  if (help_b)
    {
      identify (stdout);
      usage ();
      if (verbose_global_b)
	dir_info (stdout);
      exit (0);
    }
}

int
main (int argc, char **argv)
{
  setup_localisation ();
  setup_paths ();
  parse_argv (argc, argv);
  identify (stderr);
  initialize_kpathsea (argv[0]);

  scm_boot_guile (argc, argv, main_with_guile, 0);

  /* Unreachable */
  return 0;
}
