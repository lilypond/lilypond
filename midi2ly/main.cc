//
// main.cc -- implement  main () entry point
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#include <stdlib.h>
#include <iostream.h>
#include <assert.h>
#include <locale.h>
#include "config.h"
#include "string-convert.hh"
#include "getopt-long.hh"
#include "file-path.hh"
#include "duration-convert.hh"
#include "source.hh"

#include "midi2ly-global.hh"
#include "midi-score-parser.hh"
#include "lilypond-item.hh"
#include "lilypond-score.hh"

#if HAVE_GETTEXT
#include <libintl.h>
#endif

bool testing_level_global;

// ugh
String filename_str_g;

// ugh
Lilypond_score* lilypond_score_l_g = 0;

bool no_timestamps_b_g = false;
bool no_rests_b_g = false;

Sources source;

static File_path path;

Verbose level_ver = NORMAL_ver;


void
identify()
{
cout << midi2ly_version_str() << endl;

}

void
version ()
{
  identify ();
  cout << '\n';
  cout << _f (""
  "This is free software.  It is covered by the GNU General Public License,\n"
  "and you are welcome to change it and/or distribute copies of it under\n"
  "certain conditions.  Invoke as `%s --warranty' for more information.\n", 
    "midi2ly");
  cout << endl;

  cout << _f ("Copyright (c) %s by", "1996--2001");
  cout << "Han-Wen Nienhuys <hanwen@cs.uu.nl>\n"
       << "Jan Nieuwenhuizen <janneke@gnu.org>\n";
}

void
notice()
{
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

/*
 Internationalisation kludge in two steps:
   * use _i () to get entry in POT file
   * call gettext () explicitely for actual "translation"
 */
Long_option_init long_option_init_a[] =
{
  {0, "no-quantify", 'b', _i ("write exact durations, e.g.: a4*385/384")},
  {0, "debug", 'd', _i ("enable debugging output")},
  {0, "help", 'h', _i ("this help")},
  {_i ("ACC[:MINOR]"), "key", 'k', _i ("set key: ACC +sharps/-flats; :1 minor")},
  {0, "no-silly", 'n', _i ("don't output tuplets, double dots or rests, smallest is 32")},
  {_i ("FILE"), "output", 'o', _i ("set FILE as default output")},
  {0, "no-tuplets", 'p', _i ("don't output tuplets")},
  {0, "quiet", 'q', _i ("be quiet")},
  {0, "no-rests", 'r', _i ("don't output rests or skips")},
  {_i ("DUR"), "smallest", 's', _i ("set smallest duration")},
  {0, "no-timestamps", 'T', _i ("don't timestamp the output")},
  {0, "version", 'V', _i ("print version number")},
  {0, "verbose", 'v', _i ("be verbose")},
  {0, "warranty", 'w', _i ("show warranty and copyright")},
  {0, "no-double-dots", 'x', _i ("assume no double dotted notes")},
  {0,0,0, 0}
};

void
usage()
{
  cout << _f ("Usage: %s [OPTION]... [FILE]", "midi2ly");
  cout << '\n';
  cout << _ ("Translate MIDI-file to lilypond");
  cout << '\n';
  cout << '\n';
  cout << _ ("Options:");
  cout << '\n';
  cout << Long_option_init::table_str (long_option_init_a) << endl;

  cout << _f ("Report bugs to %s", "bug-gnu-music@gnu.org") << endl;
}

void
show_settings ()
{
  LOGOUT (VERBOSE_ver) << "\n";
  LOGOUT (VERBOSE_ver) << _f ("no_double_dots: %d\n", 
    Duration_convert::no_double_dots_b_s);
  LOGOUT (VERBOSE_ver) << _f ("no_rests: %d\n", 
    no_rests_b_g);
  LOGOUT (VERBOSE_ver) << _f ("no_quantify_b_s: %d\n", 
    Duration_convert::no_quantify_b_s);
  LOGOUT (VERBOSE_ver) << _f ("no_smaller_than: %d (1/%d)\n", 
    Duration_convert::no_smaller_than_i_s,
    Duration_convert::type2_i (Duration_convert::no_smaller_than_i_s));
  LOGOUT (VERBOSE_ver) << _f ("no_tuplets: %d\n", 
    Duration_convert::no_tuplets_b_s);
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
  Lilypond_key key (0, 0);

 
  Getopt_long getopt_long (argc_i, argv_sz_a, long_option_init_a);

  String output_str;
  while (Long_option_init const* long_option_init_p = getopt_long())
    switch (long_option_init_p->shortname_ch_)
      {
      case 'b':
	Duration_convert::no_quantify_b_s = true;
	break;
      case 'd':
	level_ver = DEBUG_ver;
	break;
      case 'h':
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
	Duration_convert::no_tuplets_b_s = true;
	Duration_convert::no_smaller_than_i_s = 5;
	no_rests_b_g = true;
	break;
      case 'o':
	output_str = getopt_long.optional_argument_ch_C_;
	break;
      case 'p':
	Duration_convert::no_tuplets_b_s = true;
	break;
      case 'q':
	level_ver = QUIET_ver;
	break;
      case 'r':
	no_rests_b_g = true;
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

      case 'V':
	version ();
	exit (0);
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
      show_settings ();
      filename_str_g = arg_sz;
      Midi_score_parser midi_parser;
      Lilypond_score* score_p = midi_parser.parse (arg_sz, &source);

      if (!score_p)
	return 1;

      // if given on command line: override
      if (key_override_b || !score_p->lilypond_key_l_)
	score_p->lilypond_key_l_ = &key;
      lilypond_score_l_g = score_p;
      score_p->process();

      if (!output_str.length_i ())
	{
	  Path p = split_path (arg_sz);

	  output_str = p.base + p.ext + ".ly";
	}

      score_p->output (output_str);
      delete score_p;
    }
  return 0;
}
