/*
  main.cc -- implement main: entrypoints

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdlib.h>
#include <iostream.h>
#include <assert.h>
#include <locale.h>
#include "proto.hh"
#include "dimensions.hh"
#include "plist.hh"
#include "getopt-long.hh"
#include "misc.hh"
#include "string.hh"
#include "main.hh"
#include "file-path.hh"
#include "config.hh"
#include "file-results.hh"
#include "debug.hh"
#include "ps-lookup.hh"
#include "tex-lookup.hh"
#include "lily-guile.hh"

#if HAVE_GETTEXT
#include <libintl.h>
#endif


bool version_ignore_global_b = false;
bool no_paper_global_b = false;
bool no_timestamps_global_b = false;
bool find_quarts_global_b = false;
String default_outname_base_global =  "lelie";
int default_count_global;
File_path global_path;

Ps_lookup ps_lookup;
Tex_lookup tex_lookup;
Lookup* global_lookup_l = &tex_lookup;

bool experimental_features_global_b = false;
bool dependency_global_b = false;

int exit_status_i_;

String distill_inname_str (String name_str, String& ext_r);

Long_option_init theopts[] = {
  {0, "about", 'a'},
  {1, "output", 'o'},
  {0, "warranty", 'w'},
  {0, "help", 'h'},
  {0, "test", 't'},
  {0, "debug", 'D'},
  {1, "init", 'i'},
  {1, "include", 'I'},
  {0, "no-paper", 'M'},
  {0, "dependencies", 'd'},
  {0, "no-timestamps", 'T'},
  {0, "find-fourths", 'Q'},
  {0, "ignore-version", 'V'},
  {0,0,0}
};

void
usage ()
{
  cout << _f ("Usage: %s [OPTION]... [FILE]...", "lilypond") << '\n';
  cout << _ ("Typeset music and or play MIDI from FILE or <stdin>");
  cout << '\n';
  cout << '\n';
  cout << _ ("Options:");
  cout << '\n';
  cout  << _ (
    "  -a, --about            about LilyPond\n"
    );
  cout  << _ (
    "  -D, --debug            enable debugging output\n"
    );
  cout  << _ (
    "  -d, --dependencies     write Makefile dependencies for every input file\n"
    );
  cout  << _ (
    "  -I, --include=DIR      add DIR to search path\n"
    );
  cout  << _ (
    "  -i, --init=FILE        use FILE as init file\n"
    );
  cout  << _ (
    "  -h, --help             this help\n"
    );
  cout  << _ (
    "  -M, --no-paper         produce midi output only\n"
    );
  cout  << _ (
    "  -o, --output=FILE      set FILE as default output base\n"
    );
  cout  << _ (
    "  -Q, --find-fourths     show all intervals greater than a fourth\n"
    );
  cout  << _ (
    "  -t, --test             switch on experimental features\n"
    );
  cout  << _ (
    "  -T, --no-timestamps    don't timestamp the output\n"
    );
  cout  << _ (
    "  -V, --ignore-version   ignore mudela version\n"
    );
  cout  << _ (
    "  -w, --warranty         show warranty and copyright\n"
    );
  cout << '\n';
  cout << _ ("GNU LilyPond was compiled with the following settings:");
  cout << '\n';
  cout <<
#ifdef NDEBUG
    "NDEBUG "
#endif
#ifdef NPRINT
    "NPRINT "
#endif
#ifdef STRING_UTILS_INLINED
    "STRING_UTILS_INLINED "
#endif
        "datadir=" DIR_DATADIR
	" "
        "localedir=" DIR_LOCALEDIR

    "\n";

  ;
}

void
about ()
{
  cout << '\n';
  cout << 
  #include "BLURB.hh"
  cout << '\n';
  cout << _ ("GNU LilyPond is Free software, see --warranty");
  cout << '\n';
  cout << '\n';
  cout << _f ("Copyright (c) %s by", "1996, 1997, 1998");
  cout << '\n';
  cout << "  " + _ ("Han-Wen Nienhuys <hanwen@cs.uu.nl>") + "\n";
  cout << "  " + _ ("Jan Nieuwenhuizen <janneke@gnu.org>") + "\n";
  cout << '\n';
}

void
notice ()
{
  cout << '\n';
  cout << _ ("GNU LilyPond -- The GNU Project music typesetter");
  cout << '\n';
  cout << _f ("Copyright (c) %s by", "1996, 1997, 1998");
  cout << '\n';
  cout << "  " + _ ("Han-Wen Nienhuys <hanwen@cs.uu.nl>") + "\n";
  cout << "  " + _ ("Jan Nieuwenhuizen <janneke@gnu.org>") + "\n";
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
identify ()
{
  *mlog << get_version_str () << endl;
}

void 
guile_init ()
{
#ifdef   HAVE_LIBGUILE
   gh_eval_str ("(define (add-column p) (display \"adding column (in guile): \") (display p) (newline))");
#endif
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

  if (!prefix_directory.empty_b())
    {
      global_path.add (prefix_directory + "/share/lilypond/ly/");
      global_path.add (prefix_directory + "/share/lilypond/afm/");
    }
  else
    {
      global_path.add (String (DIR_DATADIR) + "/ly/");
      global_path.add (String (DIR_DATADIR) + "/afm/");  
    }
}



int
main_prog (int argc, char **argv)
{
  guile_init ();
  identify ();
  call_constructors ();
  debug_init ();		// should be first

  setup_paths ();

  String init_str;
  String outname_str;

  Getopt_long oparser (argc, argv,theopts);
  while (Long_option_init const * opt = oparser ())
    {
      switch (opt->shortname)
	{
	case 't':
	  experimental_features_global_b = true;
	  global_lookup_l = &ps_lookup;
	  break;
	case 'o':
	  outname_str = oparser.optional_argument_ch_C_;
	  break;
	case 'w':
	  notice ();
	  exit (0);
	  break;
	case 'Q':
	  find_quarts_global_b = true;
	  break;
	case 'I':
	  global_path.push (oparser.optional_argument_ch_C_);
	  break;
	case 'i':
	  init_str = oparser.optional_argument_ch_C_;
	  break;
	case 'a':
	  about ();
	  exit (0);
	case 'h':
	  usage ();
	  exit (0);
	  break;
	case 'V':
	  version_ignore_global_b = true;
	  break;
	case 'd':
	  dependency_global_b = true;
	  break; 
	case 'D':
	  set_debug (true);
	  break;
	case 'M':
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

  default_outname_base_global = "lelie";

  

  int p=0;
  const char *arg ;
  while ((arg= oparser.get_next_arg ()))
    {
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
      if (outname_str.length_i ())
	default_outname_base_global = outname_str;
      if (init_str.length_i ())
	i = init_str;
      else
	i = "init" + i;
      do_one_file (i, f);
      p++;
    }
  if (!p)
    {
      String i;
      if (init_str.length_i ())
	i = init_str;
      else
	i = "init.ly";
      default_outname_base_global = "-";
      if (outname_str.length_i ())
	default_outname_base_global = outname_str;
      do_one_file (i, default_outname_base_global);
    }

  return exit_status_i_;
}

/*
  urg: make input file name: 

  input: file name

  output: file name with added default extension. "" is stdin.
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
	  if (ext_r.empty_b ())
	    {
	      ext_r = ".fly";
	      if (global_path.find (a+b+c+ext_r).empty_b ())
		ext_r = ".ly";
	    }
	  str = a+b+c+ext_r;
	}
    }
  else 
    {
      str = "-";
      ext_r = ".ly";
    }
  return str;
}


#ifdef HAVE_LIBGUILE
int
main (int argc, char **argv)
{
  gh_enter (argc, argv, (void(*)())main_prog);
  return exit_status_i_;
}

#else
int main (int argc, char **argv)
{
  return main_prog (argc, argv);
}

#endif
