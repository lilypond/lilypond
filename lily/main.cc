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

Sources* source_l_g = 0;
bool only_midi = false;
bool version_ignore_b_ = false;
int exit_status_i_;

void destill_inname (String &name_str_r);

Long_option_init theopts[] = {
  {1, "output", 'o'},
  {0, "warranty", 'w'},
  {0, "help", 'h'},
  {0, "debug", 'd'},
  {1, "init", 'i'},
  {1, "include", 'I'},
  {0, "midi", 'M'},
  {0, "ignore-version", 'V'},
  {0,0,0}
};

void
usage()
{
  cout <<
    "Usage: lilypond [options] [mudela-file]\n"
    "Typeset and or produce midi output from mudela-file or stdin\n"
    "\n"
    "Options:\n"
    "  -d, --debug            enable debugging output\n"
    "  -I, --include=DIR      add DIR to search path\n"
    "  -i, --init=FILE        use FILE as init file\n"
    "  -h, --help             this help\n"
    "  -w, --warranty         show warranty and copyright\n"
    "  -o, --output=FILE      set FILE as default output\n"
    "  -M, --midi             produce midi output only\n"
    "  -V, --ignore-version   ignore mudela version\n"
    "\n"
    "GNU LilyPond was compiled with the following settings:\n"
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
    "\n"
    "GNU LilyPond -- The GNU Project music typesetter.\n"
    "Copyright 1996,97 by\n"
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
    "USA.\n";
}


static File_path path;

void
do_one_file (String init_str, String file_str)
{
  if (init_str && "" == path.find (init_str)) 
    {
      error ("Can not find `" + init_str +"\'");
      return ;
    }
  if (file_str && !path.find (file_str))
    {
      error ("Can not find `" + file_str + "'");
      return ;
    }
  
  Sources sources;
  source_l_g = &sources; 
  source_l_g->set_path (&path);
  {
    My_lily_parser parser (source_l_g);
    parser.set_version_check (version_ignore_b_);
    parser.parse_file (init_str, file_str);
    exit_status_i_ |= parser.error_level_i_;
  }
  do_scores();
  source_l_g = 0;
}

int
main (int argc, char **argv)
{    
  debug_init();		// should be first


  // must override (come before) "/usr/local/share/lilypond"!
  char const * env_l=getenv ("LILYINCLUDE");
  if (env_l) 
    {
      path.add (env_l);
    }
  path.add ("");
  path.add (String (DIR_DATADIR) + "/init/");
  
  path.push (DIR_DATADIR);

  Getopt_long oparser (argc, argv,theopts);
  cout << get_version_str() << endl;
  String init_str ("symbol.ly");
  
  while (Long_option_init const * opt = oparser()) 
    {
      switch (opt->shortname)
	{
	case 'o':
	  set_default_output (oparser.optional_argument_ch_C_);
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
	  version_ignore_b_ = true;
	  break;
	case 'd':
	  set_debug (true);
	  break;
	case 'M':
	  only_midi = true;
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
	  if (!d) 
	    d = ".ly";
	  name_str_r = a+b+c+d;
	}
    }
  else name_str_r = "";   
}

