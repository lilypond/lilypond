/*
  main.cc -- implement main: entrypoints

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdlib.h>
#include <iostream.h>
#include <assert.h>
#include <locale.h>

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
#include "debug.hh"
#include "lily-guile.hh"
#include "paper-def.hh"
#include "midi-def.hh"
#include "global-ctor.hh"
#include "kpath.hh"


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

/* Do not write timestamps in output? */
bool no_timestamps_global_b = false;

/* Selected output format.
   One of tex, ps, scm, as. */
String output_format_global = "tex";

/* Current output name. */
String output_name_global;

/* Run in safe mode? -- FIXME: should be re-analised */
bool safe_global_b = false;

/* Verbose progress indication? */
bool verbose_global_b = false;



/*
  Misc. global stuff.
 */


All_font_metrics *all_fonts_global_p;
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
  {_i ("EXT"), "output-format", 'f',  _i ("use output format EXT (scm, ps, tex or as)")},
  {0, "help", 'h',  _i ("this help")},
  {_i ("FIELD"), "header", 'H',  _i ("write header field to BASENAME.FIELD")},
  {_i ("DIR"), "include", 'I',  _i ("add DIR to search path")},
  {_i ("FILE"), "init", 'i',  _i ("use FILE as init file")},
  {0, "dependencies", 'M',  _i ("write Makefile dependencies for every input file")},
  {_i ("DIR"), "dep-prefix", 'P',  _i ("prepend DIR to dependencies")},
  {0, "no-paper", 'm',  _i ("produce MIDI output only")},
  {_i ("NAME"), "output", 'o',  _i ("write output to NAME")},
  {0, "safe", 's',  _i ("inhibit file output naming and exporting")},
  {0, "no-timestamps", 'T',  _i ("don't timestamp the output")},
  {0, "version", 'v',  _i ("print version number")},
  {0, "verbose", 'V', _i("verbose")},
  {0, "warranty", 'w',  _i ("show warranty and copyright")},
  {0,0,0,0}
};

void
identify (ostream* os)
{
  *os << gnu_lilypond_version_str ();
}

void
usage ()
{
  
  /*
    No version number or newline here. It confuses help2man
   */
  cout << _f ("Usage: %s [OPTION]... [FILE]...", "lilypond");
  cout << "\n\n";
  cout << _ ("Typeset music and or play MIDI from FILE");
  cout << "\n\n";
  cout << 
_(
"LilyPond is a music typesetter.  It produces beautiful sheet music\n"
"using a high level description file as input.  LilyPond is part of \n"
"the GNU Project.\n"
);

  cout << '\n';
  cout << _ ("Options:");
  cout << '\n';
  cout << Long_option_init::table_str (options_static);
  cout << '\n';
  cout << _ ("This binary was compiled with the following options:") 
    << " " <<
#ifdef NDEBUG
    "NDEBUG "
#endif
#ifdef NPRINT
    "NPRINT "
#endif
#ifdef STRING_UTILS_INLINED
    "STRING_UTILS_INLINED "
#endif
    "\n"
    "datadir: `" DIR_DATADIR "'\n"
    "localedir: `" DIR_LOCALEDIR "'\n"
    "\n";


  cout << endl;

  cout << _f ("Report bugs to %s", "bug-gnu-music@gnu.org") << endl;
}

void
version ()
{
  identify (&cout);
  cout << '\n';
  cout << _f (""
  "This is free software.  It is covered by the GNU General Public License,\n"
  "and you are welcome to change it and/or distribute copies of it under\n"
  "certain conditions.  Invoke as `%s --warranty' for more information.\n",
    "lilypond");
  cout << endl;

  cout << _f ("Copyright (c) %s by", "1996--2001");
  cout << '\n';
  cout << "  Han-Wen Nienhuys <hanwen@cs.uu.nl>\n";
  cout << "  Jan Nieuwenhuizen <janneke@gnu.org>\n";
}

void
notice ()
{
  cout << '\n';
  cout << _ ("GNU LilyPond -- The music typesetter");
  cout << '\n';
  cout << _f ("Copyright (c) %s by", "1996--2001");
  cout << '\n';
  cout << "  Han-Wen Nienhuys <hanwen@cs.uu.nl>\n";
  cout << "  Jan Nieuwenhuizen <janneke@gnu.org>\n";
  cout << '\n';
  cout << _ (
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
	     "USA.\n");
}

void
setup_paths ()
{
  // facilitate binary distributions
  char const *env_lily = getenv ("LILYPONDPREFIX");
  String prefix_directory;
  if (env_lily)
    prefix_directory = env_lily;

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
  if (!prefix_directory.empty_b())
    {
      lily_locale_dir = prefix_directory + "/share/locale";
      bindtextdomain (name.ch_C (), lily_locale_dir.ch_C());
    }
  else
    bindtextdomain (name.ch_C (), DIR_LOCALEDIR);
  textdomain (name.ch_C ());
#endif

  global_path.add ("");
  // must override (come before) "/usr/local/share/lilypond"!
  char const *env_sz = getenv ("LILYINCLUDE");
  if (env_sz)
    global_path.parse_path (env_sz);


  /*
    Should use kpathsea, this is getting out of hand.  
   */
  char *suffixes[] = {"ly", "afm", "scm", "tfm", "ps", 0};
  String prefix = prefix_directory;
  if (prefix.empty_b ())
    prefix =  DIR_DATADIR;
  for (char **s = suffixes; *s; s++)
    {
      String p =  prefix + to_str ('/') + String (*s);
      global_path.add (p);

#if !KPATHSEA
      /* Urg: GNU make's $(word) index starts at 1 */
      int i  = 1;
      while (global_path.try_add (p + to_str (".") + to_str (i)))
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
	  if (!global_path.find (p.str ()).empty_b ())
	      break;
	}
      /* Reshuffle extension */
      p = split_path (p.str ());
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
main_prog (int, char**)
{
  /*
    need to do this first. Engravers use lily.scm contents.
   */
  init_lily_guile ();
  if (verbose_global_b)
    progress_indication ("\n");
  read_lily_scm_file ("lily.scm");
  cout << endl;

  call_constructors ();
  all_fonts_global_p = new All_font_metrics (global_path.str ());

  int p=0;
  const char *arg ;
  while ((arg = oparser_p_static->get_next_arg ()) || p == 0)
    {
      String infile;
      
      if (arg)
	infile = arg;
      else
	infile = "-";
	
      // What/when was this supposed to do?
      // It looks like it reset the outname_str_global for every new
      // file, but only if user didn't specify a outname?  Huh?
      // if (outname_str_global == "")
      {
	Midi_def::reset_score_count ();
	Paper_def::reset_score_count ();
      }

      Path inpath = distill_inname (infile);

      /* By default, use base name of input file for output file name */
      Path outpath = inpath;
      if (inpath.str () != "-")
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
      output_name_global = outpath.str ();
      do_one_file (init, inpath.str ());
      output_name_global = save_output_name_global;
      
      p++;
    }
  delete oparser_p_static;
  exit (exit_status_global);
}


int
main (int argc, char **argv)
{
  debug_init ();		// should be first (can see that; but Why?)
  setup_paths ();

  /*
    prepare guile for heavy mem usage.

    putenv is POSIX, setenv is BSD 4.3
   */
  putenv ("GUILE_INIT_SEGMENT_SIZE_1=4194304");
  putenv ("GUILE_MAX_SEGMENT_SIZE=8388608");

  ly_init_kpath (argv[0]);
  
  oparser_p_static = new Getopt_long(argc, argv, options_static);
  while (Long_option_init const * opt = (*oparser_p_static)())
    {
      switch (opt->shortname_ch_)
	{
	case 'v':
	  version ();
	  exit (0);		// we print a version anyway.
	  break;
	case 'o':
	  {
	    String s = oparser_p_static->optional_argument_ch_C_;
	    Path p = split_path (s);
	    if (p.ext.empty_b ())
	      p.ext = format_to_ext (output_format_global);
	    output_name_global = p.str ();
	  }
	  break;
	case 'w':
	  notice ();
	  exit (0);
	  break;
	case 'f':
	    output_format_global = oparser_p_static->optional_argument_ch_C_;
	  break;
	case 'P':
	    dependency_prefix_global = oparser_p_static->optional_argument_ch_C_;
	  break;
	case 'H':
	  dump_header_fieldnames_global.push (oparser_p_static->optional_argument_ch_C_);
	  break;
	case 'I':
	  global_path.push (oparser_p_static->optional_argument_ch_C_);
	  break;
	case 'i':
	  init_name_global = oparser_p_static->optional_argument_ch_C_;
	  break;
	case 'h':
	  usage ();
	  exit (0);
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
	case 'T':
	  no_timestamps_global_b = true;
	  break;
	default:
	  assert (false);
	  break;
	}
    }
  identify (&cerr);

#ifdef WINNT
  gh_enter (argc, argv, main_prog);
#else
  gh_enter (argc, argv, (void(*)(int, char**))main_prog);
#endif

  return 0;			// unreachable
}


