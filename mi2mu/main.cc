//
// main.cc -- implement silly main() entry point
// should have Root class.
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include <assert.h>
#include "string-convert.hh"
#include "lgetopt.hh"
#include "path.hh"
#include "duration-convert.hh"
#include "source.hh"

#include "mi2mu-global.hh"
#include "midi-score-parser.hh"
#include "mudela-item.hh"
#include "mudela-score.hh"
#include "version.hh"

// ugh
String filename_str_g;

// ugh
Mudela_score* mudela_score_l_g = 0;

Sources source;

static File_path path;

Verbose level_ver = NORMAL_ver;

void
usage()
{
  LOGOUT(NORMAL_ver) <<
	_("Usage: mi2mu [options] midi-file\n"
  "Translate midi-file to mudela\n"
  "\n"
  "Options:\n"
  "  -b, --no-quantify      write exact durations, e.g.: a4*385/384\n"
  "  -d, --debug            print lots of debugging stuff\n"
  "  -h, --help             this help\n"
  "  -I, --include=DIR      add DIR to search path\n"
  "  -k, --key=ACC[:MINOR]  set key: ACC +sharps/-flats; :1 minor\n"
  "  -n, --no-silly         assume no plets or double dots, smallest is 32\n"
  "  -o, --output=FILE      set FILE as default output\n"
  "  -p, --no-plets         assume no plets\n"
  "  -q, --quiet            be quiet\n"
  "  -s, --smallest=N       assume no shorter (reciprocal) durations than N\n"
  "  -v, --verbose          be verbose\n"
  "  -w, --warranty         show warranty and copyright\n"
  "  -x, --no-double-dots   assume no double dotted notes\n")
  ;
}

void
identify()
{
  LOGOUT(NORMAL_ver) << mi2mu_version_str() << endl;
}

void
notice()
{
  LOGOUT(NORMAL_ver) <<
  _("\n"
  "Mi2mu, translate midi to mudela.\n"
  "Copyright (C) 1997 by\n"
  "  Jan Nieuwenhuizen <jan@digicash.com>\n"
  "  Han-Wen Nienhuys <hanwen@stack.nl>\n"
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

int
main (int argc_i, char* argv_sz_a[])
{
  Mudela_key key (0, 0);

  Long_option_init long_option_init_a[] =
    {
	{0, "no-quantify", 'b'},
	{0, "debug", 'd'},
	{0, "help", 'h'},
	{1, "key", 'k'},
	{0, "no-silly", 'n'},
	{1, "output", 'o'},
	{0, "no-plets", 'p'},
	{0, "quiet", 'q'},
	{1, "smallest", 's'},
	{0, "verbose", 'v'},
	{0, "warranty", 'w'},
	{0, "no-double-dots", 'x'},
	{0,0,0}
  };
  Getopt_long getopt_long (argc_i, argv_sz_a, long_option_init_a);

  String output_str;
  while (Long_option_init const* long_option_init_p = getopt_long())
	switch (long_option_init_p->shortname)
	  {
	case 'b':
	    Duration_convert::no_quantify_b_s = true;
	    break;
	case 'd':
	    level_ver = DEBUG_ver;
	    break;
	case 'h':
	    identify();
	    usage();
	    exit (0);
	    break;
//	case 'I':
//	    path->push (getopt_long.optional_argument_ch_C_);
//	    break;
	case 'k':
	  {
	    String str = getopt_long.optional_argument_ch_C_;
	    int i = str.index_i (':');
	    i = (i >=0 ? i : str.length_i ());
	    key.accidentals_i_ = String_convert::dec2_i (str.left_str (i));
	    key.minor_i_ = (int)(bool)String_convert::dec2_i (str.cut (i + 1,1));
	    break;
	  }
	case 'n':
	    Duration_convert::no_double_dots_b_s = true;
	    Duration_convert::no_triplets_b_s = true;
	    Duration_convert::no_smaller_than_i_s = 5;
	    break;
	case 'o':
	    output_str = getopt_long.optional_argument_ch_C_;
	    break;
	case 'p':
	    Duration_convert::no_triplets_b_s = true;
	    break;
	case 'q':
	    level_ver = QUIET_ver;
	    break;
	case 's':
	  {
		int i = String_convert::dec2_i (getopt_long.optional_argument_ch_C_);
		if (!i)
		  {
		    identify();
		    usage();
		    exit (2); //usage
		  }
		Duration_convert::no_smaller_than_i_s =
		  Duration_convert::i2_type(i);
	      }
	    break;
	case 'v':
	    level_ver = VERBOSE_ver;
	    break;
	case 'w':
	    identify();
	    notice();
	    exit (0);
	    break;
	case 'x':
	    Duration_convert::no_double_dots_b_s = true;
	    break;
	default:
	    assert (0);
	    break;
	    }

  // flag -q must be checked first
  identify();

  path.add ("");
  source.set_binary (true);
  source.set_path (&path);

  char const* arg_sz = 0;
  while ( (arg_sz = getopt_long.get_next_arg ()))
    {
	filename_str_g = arg_sz;
	Midi_score_parser midi_parser;
	Mudela_score* score_p = midi_parser.parse (arg_sz, &source);

	if (!score_p)
	  return 1;

	// if given on command line: override
	score_p->mudela_key_l_ = &key;
	mudela_score_l_g = score_p;
	score_p->process();

	if (!output_str.length_i ())
	  {
	    String d, dir, base, ext;
	    split_path (arg_sz, d, dir, base, ext);
	    output_str = base + ext + ".ly";
	  }

	score_p->output (output_str);
	delete score_p;
    }
  return 0;
}
