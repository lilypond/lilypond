/*
  main.cc -- implement main: entrypoints

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <iostream.h>
#include <assert.h>
#include "proto.hh"
#include "plist.hh"
#include "lgetopt.hh"
#include "misc.hh"
#include "string.hh"
#include "main.hh"
#include "path.hh"
#include "config.hh"
#include "source.hh"
#include "debug.hh"
#include "my-lily-parser.hh"

static bool version_ignore_b = false;
Sources* source_global_l = 0;
bool no_paper_global_b = false;

bool experimental_features_global_b = false;
bool postscript_global_b = true;
int exit_status_i_;

void destill_inname (String &name_str_r);

Long_option_init theopts[] = {
  {1, "output", 'o'},
  {0, "warranty", 'w'},
  {0, "help", 'h'},
  {0, "test", 't'},
  {0, "debug", 'd'},
  {1, "init", 'i'},
  {1, "include", 'I'},
  {0, "no-paper", 'M'},
  {0, "no-postscript", 'P'},
  {0, "ignore-version", 'V'},
  {0,0,0}
};

void
usage()
{
  cout <<
    _("Usage: lilypond [options] [mudela-files]\n"
    "Typeset and or produce midi output from mudela-file or stdin\n"
    "\n"
    "Options:\n"
    "  -d, --debug            enable debugging output\n"
    "  -I, --include=DIR      add DIR to search path\n"
    "  -i, --init=FILE        use FILE as init file\n"
    "  -h, --help             this help\n"
    "  -w, --warranty         show warranty and copyright\n"
    "  -o, --output=FILE      set FILE as default output\n"
    "  -t, --test             switch on experimental features\n"
    "  -M, --no-paper         produce midi output only\n"
    "  -V, --ignore-version   ignore mudela version\n"
    "  -P, --no-postscript    don't use PostScript\n"
    "\n"
    "GNU LilyPond was compiled with the following settings:\n")
#ifdef NDEBUG
    "NDEBUG "
#endif
#ifdef NPRINT
    "NPRINT "
#endif
#ifdef STRING_UTILS_INLINED
    "STRING_UTILS_INLINED "
#endif
    "datadir= " DIR_DATADIR

    "\n";

  ;


}

void
notice()
{
  cout <<
    _("\n"
    "GNU LilyPond -- The GNU Project music typesetter.\n"
    "Copyright 1996, 97, 98 by\n"
    "  Han-Wen Nienhuys <hanwen@stack.nl>\n"
    "  Jan Nieuwenhuizen <jan@digicash.com>\n"
    "\n"
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


static File_path path;

void
do_one_file (String init_str, String file_str)
{
  if (init_str.length_i () && path.find (init_str).empty_b ())
    {
      error (_("Can not find `") + init_str +"\'");
      return ;
    }
  if (file_str.length_i () && path.find (file_str).empty_b ())
    {
      error (_("Can not find `") + file_str + "'");
      return ;
    }

  Sources sources;
  source_global_l = &sources;
  source_global_l->set_path (&path);
  {
    My_lily_parser parser (source_global_l);
    parser.set_version_check (version_ignore_b);
    parser.parse_file (init_str, file_str);
    
    if (file_str.length_i() && file_str[0] != '-')
      {
	String a,b,c,d;
	split_path (file_str, a, b, c, d);
	default_outname_base_global = c;
      }
    else
      default_outname_base_global = "lelie";
  
    if (parser.error_level_i_)
      {
	exit_status_i_  = 1;
      }
    else
      do_scores();
    clear_scores ();
  }
  source_global_l = 0;
}

void
identify ()
{
  cout << get_version_str() << endl;
}

int
main (int argc, char **argv)
{
  identify ();
  debug_init();		// should be first


  // must override (come before) "/usr/local/share/lilypond"!
  char const *env_sz = getenv ("LILYINCLUDE");
  if (env_sz)
    path.parse_path (env_sz);

  path.add ("");
  path.add (String (DIR_DATADIR) + "/init/");

  path.push (DIR_DATADIR);

  Getopt_long oparser (argc, argv,theopts);
  String init_str ("lily-init.ly");

  while (Long_option_init const * opt = oparser())
    {
      switch (opt->shortname)
	{
	case 't':
	  experimental_features_global_b = true;
	  break;
	case 'o':
	  default_outname_base_global = oparser.optional_argument_ch_C_;
	  break;
	case 'w':
	  notice();
	  exit (0);
	  break;
	case 'I':
	  path.push (oparser.optional_argument_ch_C_);
	  break;
	case 'i':
	  init_str = oparser.optional_argument_ch_C_;
	  break;
	case 'h':
	  usage();
	  exit (0);
	  break;
	case 'V':
	  version_ignore_b = true;
	  break;
	case 'P':
	  postscript_global_b = false;
	  break;
	case 'd':
	  set_debug (true);
	  break;
	case 'M':
	  no_paper_global_b = true;
	  break;
	default:
	  assert (false);
	  break;
	}
    }

  int p=0;
  const char *arg ;
  while ((arg= oparser.get_next_arg()))
    {
      String f (arg);
      destill_inname (f);
      do_one_file (init_str,f);
      p++;
    }
  if (!p)
    {
      do_one_file (init_str, "");
    }

  return exit_status_i_;
}

/// make input file name: add default extension. "" is stdin.
void
destill_inname (String &name_str_r)
{
  if (name_str_r.length_i())
    {
      if (name_str_r[ 0 ] != '-')
	{
	  String a,b,c,d;
	  split_path (name_str_r,a,b,c,d);

	  // add extension if not present.
	  if (d.empty_b ())
	    d = ".ly";
	  name_str_r = a+b+c+d;
	}
    }
  else name_str_r = "";
}
