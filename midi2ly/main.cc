//
// main.cc -- implement silly main () entry point
// should have Root class.
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#include <assert.h>
#include <locale.h>
#include "config.hh"
#include "string-convert.hh"
#include "getopt-long.hh"
#include "file-path.hh"
#include "duration-convert.hh"
#include "source.hh"

#include "midi2ly-global.hh"
#include "midi-score-parser.hh"
#include "mudela-item.hh"
#include "mudela-score.hh"

#if HAVE_GETTEXT
#include <libintl.h>
#endif


// ugh
String filename_str_g;

// ugh
Mudela_score* mudela_score_l_g = 0;

bool no_timestamps_b_g = false;

Sources source;

static File_path path;

Verbose level_ver = NORMAL_ver;

void
usage()
{
  cout << _f ("Usage: %s [OPTION]... [FILE]", "midi2ly");
  cout << '\n';
  cout << _ ("Translate midi-file to mudela");
  cout << '\n';
  cout << '\n';
  cout << _ ("Options:");
  cout << '\n';
  cout << _ (
  "  -b, --no-quantify      write exact durations, e.g.: a4*385/384\n"
  );
  cout << _ (
  "  -D, --debug            enable debugging output\n"
  );
  cout << _ (
  "  -h, --help             this help\n"
  );
  cout << _ (
  "  -I, --include=DIR      add DIR to search path\n"
  );
  cout << _ (
  "  -k, --key=ACC[:MINOR]  set key: ACC +sharps/-flats; :1 minor\n"
  );
  cout << _ (
  "  -n, --no-silly         assume no plets or double dots, smallest is 32\n"
  );
  cout << _ (
  "  -o, --output=FILE      set FILE as default output\n"
  );
  cout << _ (
  "  -p, --no-plets         assume no plets\n"
  );
  cout << _ (
  "  -q, --quiet            be quiet\n"
  );
  cout << _ (
  "  -T, --no-timestamps    don't timestamp the output\n"
  );
  cout << _ (
  "  -s, --smallest=N       assume no shorter (reciprocal) durations than N\n"
  );
  cout << _ (
  "  -v, --verbose          be verbose\n"
  );
  cout << _ (
  "  -w, --warranty         show warranty and copyright\n"
  );
  cout << _ (
  "  -x, --no-double-dots   assume no double dotted notes\n"
  );
  ;
}

void
identify()
{
  LOGOUT(NORMAL_ver) << midi2ly_version_str() << endl;
}

void
notice()
{
  LOGOUT(NORMAL_ver) << '\n';
  LOGOUT(NORMAL_ver) << _ ("Midi2ly, translate midi to mudela");
  LOGOUT(NORMAL_ver) << '\n';
  LOGOUT(NORMAL_ver) << _f ("Copyright (c) %s by", "1997, 1998");
  LOGOUT(NORMAL_ver) << '\n';
  LOGOUT(NORMAL_ver) << "  " + _ ("Han-Wen Nienhuys <hanwen@cs.uu.nl>") + "\n";
  LOGOUT(NORMAL_ver) << "  " + _ ("Jan Nieuwenhuizen <janneke@gnu.org>") + "\n";
  LOGOUT(NORMAL_ver) << '\n';
  LOGOUT(NORMAL_ver) << _ (
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

#if HAVE_GETTEXT
  setlocale (LC_ALL, ""); /* enable locales */
  setlocale (LC_NUMERIC, "C"); /* musn't have comma's in output */
  String name (PACKAGE);
  name.to_lower ();
  bindtextdomain (name.ch_C (), DIR_LOCALEDIR);
  textdomain (name.ch_C ()) ;
#endif

  bool key_override_b = false;
  Mudela_key key (0, 0);

  Long_option_init long_option_init_a[] =
    {
	{0, "no-quantify", 'b'},
	{0, "debug", 'D'},
	{0, "help", 'h'},
	{1, "key", 'k'},
	{0, "no-silly", 'n'},
	{1, "output", 'o'},
	{0, "no-plets", 'p'},
	{0, "quiet", 'q'},
	{1, "smallest", 's'},
	{0, "no-timestamps", 'T'},
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
	case 'D':
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
	    key.minor_i_ = (int)(bool)String_convert::dec2_i (str.cut_str (i + 1, str.length_i ()));
	    key_override_b = true;
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
	case 'T':
	    no_timestamps_b_g = true;
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
	if (key_override_b || !score_p->mudela_key_l_)
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
