/*
  main.cc -- implement main: entrypoints

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdlib.h>
#include <iostream.h>
#include <assert.h>
#include <locale.h>

#include "lily-guile.hh"
#include "lily-version.hh"

#include "all-font-metrics.hh"
#include "getopt-long.hh"
#include "misc.hh"
#include "string.hh"
#include "main.hh"
#include "file-path.hh"
#include "config.h"
#include "file-results.hh"
#include "debug.hh"
#include "lily-guile.hh"
#include "paper-def.hh"
#include "midi-def.hh"
#include "global-ctor.hh"
#include "kpath.hh"


#if HAVE_GETTEXT
#include <libintl.h>
#endif



bool verbose_global_b = false;
bool no_paper_global_b = false;
bool no_timestamps_global_b = false;
bool find_old_relative_b = false;

char const* output_global_ch = "tex";
All_font_metrics *all_fonts_global_p;

String default_outname_base_global =  "lelie";
String outname_str_global;
String init_str_global;

int default_count_global;
File_path global_path;

bool safe_global_b = false;
bool experimental_features_global_b = false;
bool dependency_global_b = false;

int exit_status_i_;

Getopt_long * oparser_global_p = 0;

String distill_inname_str (String name_str, String& ext_r);

/*
 Internationalisation kludge in two steps:
   * use _i () to get entry in POT file
   * call gettext () explicitely for actual "translation"

 Note: these messages all start with lower case (ie, don't
       follow regular localisation guidelines).
 */
Long_option_init theopts[] = {
  {0, "debug", 'd',  _i ("enable debugging output")},
  {_i ("EXT"), "output-format", 'f',  _i ("use output format EXT (scm, ps, tex or as)")},
  {0, "help", 'h',  _i ("this help")},
  {_i ("DIR"), "include", 'I',  _i ("add DIR to search path")},
  {_i ("FILE"), "init", 'i',  _i ("use FILE as init file")},
  {0, "dependencies", 'M',  _i ("write Makefile dependencies for every input file")},
  {0, "no-paper", 'm',  _i ("produce MIDI output only")},
  {_i ("BASENAME"), "output", 'o',  _i ("write output to BASENAME[-x].extension")},
  {0, "find-old-relative", 'Q',  _i ("show all changes in relative syntax")},
  {0, "safe", 's',  _i ("inhibit file output naming and exporting")},
  {0, "no-timestamps", 'T',  _i ("don't timestamp the output")},
  {0, "test", 't',  _i ("switch on experimental features")},
  {0, "version", 'v',  _i ("print version number")},
  {0, "verbose", 'V', _i("verbose")},
  {0, "warranty", 'w',  _i ("show warranty and copyright")},
  {0,0,0, 0}
};

void
identify (ostream* os)
{
  //*os << gnu_lilypond_version_str () << endl;
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
  cout << Long_option_init::table_str (theopts);
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

  cout << _f ("Copyright (c) %s by", "1996--2000");
  cout << "Han-Wen Nienhuys <hanwen@cs.uu.nl>\n"
       << "Jan Nieuwenhuizen <janneke@gnu.org>\n";
}

void
notice ()
{
  cout << '\n';
  // GNU GNU?
  cout << _ ("GNU LilyPond -- The GNU Project music typesetter");
  cout << '\n';
  cout << _f ("Copyright (c) %s by", "1996--2000");
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
  if (prefix.empty_b ()) prefix =  DIR_DATADIR;
  for (char **s = suffixes; *s; s++)
    {
      String p =  prefix + to_str ('/') + String (*s);
      global_path.add (p);

#if !KPATHSEA
      /*
      Although kpathsea seems nice, it is not universally available 
      (GNU/Windows). 

      Compiling kpathsea seems not possible without
      (compiling) a matching tex installation.  Apart from the fact
      that I cannot get kpathsea compiled for GNU/Windows, another
      major problem is that TeX installations may be different on
      different clients, so it wouldn't work anyway.  While ugly,
      this code is simple and effective.
        -- jcn
      */

      /* Urg: GNU make's $(word) index starts at 1 */
      int i  = 1;
      while (global_path.try_add (p + to_str (".") + to_str (i)))
	i++;
#endif
    }
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
  default_outname_base_global = "lelie";
  all_fonts_global_p = new All_font_metrics (global_path.str ());
  
  int p=0;
  const char *arg ;
  while ((arg= oparser_global_p->get_next_arg ()))
    {
      
      if (outname_str_global == "")
	{
	  Midi_def::reset_default_count ();
	  Paper_def::reset_default_count ();
	}
      String f (arg);
      String i;
      f = distill_inname_str (f, i);
      if (f == "-")
	default_outname_base_global = "-";
      else
	{
	  String a,b,c,d;
	  split_path (f, a, b, c, d);
	  default_outname_base_global = c;
	}
      if (outname_str_global.length_i ())
	default_outname_base_global = outname_str_global;
      if (init_str_global.length_i ())
	i = init_str_global;
      else
	i = "init" + i;
      do_one_file (i, f);
      p++;
    }
  if (!p)
    {
      String i;
      if (init_str_global.length_i ())
	i = init_str_global;
      else
	i = "init.ly";
      default_outname_base_global = "-";
      if (outname_str_global.length_i ())
	default_outname_base_global = outname_str_global;
      do_one_file (i, default_outname_base_global);
    }
  delete oparser_global_p;
  exit( exit_status_i_);
}


int
main (int argc, char **argv)
{
  debug_init ();		// should be first
  setup_paths ();

  /*
    prepare guile for heavy mem usage.

    putenv is POSIX, setenv is BSD 4.3
   */
  putenv ("GUILE_INIT_SEGMENT_SIZE_1=4194304");
  putenv ("GUILE_MAX_SEGMENT_SIZE=8388608");

  ly_init_kpath (argv[0]);
  
  oparser_global_p = new Getopt_long(argc, argv,theopts);
  while (Long_option_init const * opt = (*oparser_global_p)())
    {
      switch (opt->shortname_ch_)
	{
	case 'v':
	  version();
	  exit (0);		// we print a version anyway.
	  break;
	case 't':
	  experimental_features_global_b = true;
	  progress_indication ("*** enabling experimental features, you're on your own now ***\n");
	  break;
	case 'o':
	  outname_str_global = oparser_global_p->optional_argument_ch_C_;
	  break;
	case 'w':
	  notice ();
	  exit (0);
	  break;
	case 'f':
	  output_global_ch = oparser_global_p->optional_argument_ch_C_;
	  break;
	case 'Q':
	  find_old_relative_b= true;
	  break;
	case 'I':
	  global_path.push (oparser_global_p->optional_argument_ch_C_);
	  break;
	case 'i':
	  init_str_global = oparser_global_p->optional_argument_ch_C_;
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
	case 'd':
	  set_debug (true);
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

/**
  make input file name from command arg.

  @input file name

  @output file name with added default extension. "" is stdin.
          in reference argument: the extension. ".ly" if none
 */
String
distill_inname_str (String name_str, String& ext_r)
{
  String str = name_str;
  if (str.length_i ())
    {
      if (str != "-")
	{
	  String a,b,c;
	  split_path (str,a,b,c,ext_r);

	  // add extension if not present.
	  char const* extensions[] = {"", ".ly", ".fly", ".sly", "", 0};
	  extensions[0] = ext_r.ch_C ();
	  for (int i = 0; extensions[i]; i++)
	    {
	      if (!global_path.find (a+b+c+extensions[i]).empty_b ())
		{
		  ext_r = extensions[i];
		  break;
		}
	    }
	  str = a+b+c+ext_r;
	  // in any case, assume (init).ly
	  if (!ext_r.length_i ())
	    ext_r = ".ly";
	}
    }
  else 
    {
      str = "-";
      ext_r = ".ly";
    }
  return str;
}

