/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "main.hh"

#include <cassert>
#include <clocale>
#include <cstring>
#include <cerrno>
#include <cstdio>
using namespace std;

#include <unistd.h>
#include <sys/types.h>
#include "config.hh"

#if HAVE_GRP_H
#include <grp.h>
#endif
#if HAVE_PWD_H
#include <pwd.h>
#endif
#if HAVE_GETTEXT
#include <libintl.h>
#endif

#include "all-font-metrics.hh"
#include "file-name.hh"
#include "freetype.hh"
#include "getopt-long.hh"
#include "global-ctor.hh"
#include "international.hh"
#include "lily-version.hh"
#include "misc.hh"
#include "output-def.hh"
#include "program-option.hh"
#include "relocate.hh"
#include "string-convert.hh"
#include "version.hh"
#include "warn.hh"

/*
 * Global options that can be overridden through command line.
 */

/* Names of header fields to be dumped to a separate file. */
vector<string> dump_header_fieldnames_global;

/* Name of initialisation file. */
string init_name_global;


/* Output formats to generate.  */
string output_format_global = "";

/* Current output name. */
string output_name_global;

/* Run in safe mode? */
bool be_safe_global = false;

/* Provide URI links to the original file */
bool point_and_click_global = true;

/* Verbose progress indication? */
bool be_verbose_global = false;

/* Scheme code to execute before parsing, after .scm init.
   This is where -e arguments are appended to.  */
string init_scheme_code_global;
string init_scheme_variables_global;

bool relocate_binary = true;


/*
 * Miscellaneous global stuff.
 */
File_path global_path;

/*
 * File globals.
 */

static char const *AUTHORS
= "  Han-Wen Nienhuys <hanwen@xs4all.nl>\n"
  "  Jan Nieuwenhuizen <janneke@gnu.org>\n";

static char const *PROGRAM_NAME = "lilypond";
static char const *PROGRAM_URL = "http://lilypond.org";

static char const *NOTICE
= _i ("This program is free software.  It is covered by the GNU General Public\n"
      "License and you are welcome to change it and/or distribute copies of it\n"
      "under certain conditions.  Invoke as `%s --warranty' for more\n"
      "information.\n");

static char const *WARRANTY
= _i ("    This program is free software; you can redistribute it and/or\n"
      "modify it under the terms of the GNU General Public License version 2\n"
      "as published by the Free Software Foundation.\n"
      "\n"
      "    This program is distributed in the hope that it will be useful,\n"
      "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
      "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n"
      "General Public License for more details.\n"
      "\n"
      "    You should have received a copy of the\n"
      "GNU General Public License along with this program; if not, write to\n"
      "the Free Software Foundation, Inc., 59 Temple Place - Suite 330,\n"
      "Boston, MA 02111-1307, USA.\n");

/* Where the init files live.  Typically:
   LILYPOND_DATADIR = /usr/share/lilypond
*/
string lilypond_datadir;

/* The jail specification: USER, GROUP, JAIL, DIR. */
string jail_spec;

/*  The option parser */
static Getopt_long *option_parser = 0;

/* Internationalisation kludge in two steps:
 * use _i () to get entry in POT file
 * call gettext () explicitly for actual "translation"  */

static Long_option_init options_static[]
= {
  {_i ("SYM[=VAL]"), "define-default", 'd',
   _i ("set Scheme option SYM to VAL (default: #t).\n"
       "Use -dhelp for help.")},

  {_i ("EXPR"), "evaluate", 'e', _i ("evaluate scheme code")},
  /* Bug in option parser: --output =foe is taken as an abbreviation
     for --output-format.  */
  {_i ("FORMATs"), "formats", 'f', _i ("dump FORMAT,...  Also as separate options:")},
  {0, "pdf", 0, _i ("generate PDF (default)")},
  {0, "png", 0, _i ("generate PNG")},
  {0, "ps", 0, _i ("generate PostScript")},
  {0, "help", 'h',  _i ("show this help and exit")},
  {_i ("FIELD"), "header", 'H',  _i ("dump header field FIELD to file\n"
				     "named BASENAME.FIELD")},
  {_i ("DIR"), "include", 'I',  _i ("add DIR to search path")},
  {_i ("FILE"), "init", 'i',  _i ("use FILE as init file")},
#if HAVE_CHROOT
  {_i ("USER, GROUP, JAIL, DIR"), "jail", 'j', _i ("chroot to JAIL, become USER:GROUP\n"
						"and cd into DIR")},
#endif
  {_i ("FILE"), "output", 'o',  _i ("write output to FILE (suffix will be added)")},
  {0, "relocate", 0, _i ("relocate using directory of lilypond program")},
  {0, "version", 'v',  _i ("show version number and exit")},
  {0, "verbose", 'V', _i ("be verbose")},
  {0, "warranty", 'w',  _i ("show warranty and copyright")},
  {0, 0, 0, 0}
};

char const *LILYPOND_DATADIR = PACKAGE_DATADIR "/" TOPLEVEL_VERSION;


/* x86 defaults to using 80-bit extended precision arithmetic. This can cause
   problems because the truncation from 80 bits to 64 bits can occur in
   unpredictable places. To get around this, we tell the x87 FPU to use only
   double precision. Note that this is not needed for x86_64 because that uses
   the SSE unit by default instead of the x87 FPU. */
#if ((defined(__x86__) || defined(__i386__)) \
  && defined(HAVE_FPU_CONTROL_H) && (HAVE_FPU_CONTROL_H == 1))

#include <fpu_control.h>
static void
configure_fpu ()
{
  fpu_control_t fpu_control = 0x027f;
  _FPU_SETCW (fpu_control);
}

#else

static void
configure_fpu ()
{
}

#endif /* defined(__x86__) || defined(__i386__) */


static void
env_var_info (FILE *out, char const *key)
{
  if (char const *value = getenv (key))
    fprintf (out, "%s=\"%s\"\n", key, value);
}

static void
dir_info (FILE *out)
{
  fputs ("\n", out);
  fprintf (out, "LILYPOND_DATADIR=\"%s\"\n", LILYPOND_DATADIR);
  env_var_info (out, "LILYPONDPREFIX");
  env_var_info (out, "LILYPOND_DATADIR");
  fprintf (out, "LOCALEDIR=\"%s\"\n", LOCALEDIR);

  fprintf (out, "\nEffective prefix: \"%s\"\n", lilypond_datadir.c_str ());

  if (relocate_binary)
    {
      env_var_info (out, "FONTCONFIG_FILE");
      env_var_info (out, "FONTCONFIG_PATH");
      env_var_info (out, "GS_FONTPATH");
      env_var_info (out, "GS_LIB");
      env_var_info (out, "GUILE_LOAD_PATH");
      env_var_info (out, "PANGO_RC_FILE");
      env_var_info (out, "PANGO_PREFIX");
      env_var_info (out, "PATH");
    }
}

static void
copyright ()
{
  /* Do not update the copyright years here, run grand-replace.sh.  */
  printf ("%s", (_f ("Copyright (c) %s by\n%s  and others.", "1996--2009",
                     AUTHORS).c_str ()));
  printf ("\n");
}

static void
identify (FILE *out)
{
  fputs (gnu_lilypond_version_string ().c_str (), out);
  fputs ("\n", out);
}

static void
notice ()
{
  identify (stdout);
  printf ("\n");
  copyright ();
  printf ("\n");
  puts (_f (NOTICE, PROGRAM_NAME).c_str ());
}

LY_DEFINE (ly_usage, "ly:usage",
	   0, 0, 0, (),
	   "Print usage message.")
{
  /* No version number or newline here.  It confuses help2man.  */
  printf ("%s", (_f ("Usage: %s [OPTION]... FILE...", PROGRAM_NAME).c_str ()));
  printf ("\n\n");
  printf ("%s", (_ ("Typeset music and/or produce MIDI from FILE.").c_str ()));
  printf ("\n\n");
  printf ("%s", (_ ("LilyPond produces beautiful music notation.").c_str ()));
  printf ("\n");
  printf ("%s", (_f ("For more information, see %s", PROGRAM_URL).c_str ()));
  printf ("\n\n");
  printf ("%s", (_ ("Options:").c_str ()));
  printf ("\n");
  printf ("%s", Long_option_init::table_string (options_static).c_str ());
  printf ("\n");
  /* Translators, please translate this string as
     "Report bugs in English via %s",
     or if there is a LilyPond users list or forum in your language
     "Report bugs in English via %s or in YOUR_LANG via URI"  */
  printf ("%s", (_f ("Report bugs via %s",
		     "http://post.gmane.org/post.php?group=gmane.comp.gnu.lilypond.bugs"
		     ).c_str ()));
  printf ("\n");
  printf ("\n");
  return SCM_UNSPECIFIED;
}

static void
warranty ()
{
  identify (stdout);
  printf ("\n");
  copyright ();
  printf ("\n");
  printf ("%s", (_ (WARRANTY).c_str ()));
}

static void
prepend_load_path (string dir)
{
  string s = "(set! %load-path (cons \"" + dir + "\" %load-path))";
  scm_c_eval_string (s.c_str ());
}

void init_global_tweak_registry ();
void init_fontconfig ();

#if HAVE_CHROOT
static void
do_chroot_jail ()
{
  /* Now we chroot, setuid/setgrp and chdir.  If something goes wrong,
     we exit (this is a security-sensitive area).  First we split
     jail_spec into its components, then we retrieve the user/group id
     (necessarily *before* chroot'ing) and finally we perform the
     actual actions.  */

  enum Jail
    {
      USER_NAME, GROUP_NAME, JAIL, DIR, JAIL_MAX
    };

  vector<string> components = string_split (jail_spec, ',');
  if (components.size () != JAIL_MAX)
    {
      error (_f ("expected %d arguments with jail, found: %u", JAIL_MAX,
		 (unsigned) components.size ()));
      exit (2);
    }

  /* Hmm.  */
  errno = 0;

  int uid;
  if (passwd * passwd = getpwnam (components[USER_NAME].c_str ()))
    uid = passwd->pw_uid;
  else
    {
      if (errno == 0)
	error (_f ("no such user: %s", components[USER_NAME]));
      else
	error (_f ("cannot get user id from user name: %s: %s",
		   components[USER_NAME],
		   strerror (errno)));
      exit (3);
    }

  /* Hmm.  */
  errno = 0;

  int gid;
  if (group * group = getgrnam (components[GROUP_NAME].c_str ()))
    gid = group->gr_gid;
  else
    {
      if (errno == 0)
	error (_f ("no such group: %s", components[GROUP_NAME]));
      else
	error (_f ("cannot get group id from group name: %s: %s",
		   components[GROUP_NAME],
		   strerror (errno)));
      exit (3);
    }

  if (chroot (components[JAIL].c_str ()))
    {
      error (_f ("cannot chroot to: %s: %s", components[JAIL],
		 strerror (errno)));
      exit (3);
    }

  if (setgid (gid))
    {
      error (_f ("cannot change group id to: %d: %s", gid, strerror (errno)));
      exit (3);
    }

  if (setuid (uid))
    {
      error (_f ("cannot change user id to: %d: %s", uid, strerror (errno)));
      exit (3);
    }

  if (chdir (components[DIR].c_str ()))
    {
      error (_f ("cannot change working directory to: %s: %s", components[DIR],
		 strerror (errno)));
      exit (3);
    }
}
#endif

static void
main_with_guile (void *, int, char **)
{
  /* Engravers use lily.scm contents, need to make Guile find it.
     Prepend onto GUILE %load-path, very ugh. */

  prepend_load_path (lilypond_datadir);
  prepend_load_path (lilypond_datadir + "/scm");

  if (be_verbose_global)
    dir_info (stderr);

  init_scheme_variables_global = "(list " + init_scheme_variables_global + ")";
  init_scheme_code_global = "(begin " + init_scheme_code_global + ")";

  ly_c_init_guile ();
  call_constructors ();
  init_fontconfig ();

  init_freetype ();
  ly_reset_all_fonts ();

  /* We accept multiple independent music files on the command line to
     reduce compile time when processing lots of small files.
     Starting the GUILE engine is very time consuming.  */

  SCM files = SCM_EOL;
  SCM *tail = &files;
  while (char const *arg = option_parser->get_next_arg ())
    {
      *tail = scm_cons (scm_from_locale_string (arg), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }

  delete option_parser;
  option_parser = 0;

#if HAVE_CHROOT
  if (!jail_spec.empty ())
    do_chroot_jail ();
#endif

  SCM result = scm_call_1 (ly_lily_module_constant ("lilypond-main"), files);
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
     Disable localisation of float values. */
  setlocale (LC_NUMERIC, "C");

  string localedir = LOCALEDIR;
  if (char const *env = getenv ("LILYPOND_LOCALEDIR"))
    localedir = env;

  bindtextdomain ("lilypond", localedir.c_str ());
  textdomain ("lilypond");
#endif
}

static void
add_output_format (string format)
{
  if (output_format_global != "")
    output_format_global += ",";
  output_format_global += format;
}

static void
parse_argv (int argc, char **argv)
{
  bool show_help = false;
  option_parser = new Getopt_long (argc, argv, options_static);
  while (Long_option_init const *opt = (*option_parser) ())
    {
      switch (opt->shortname_char_)
	{
	case 0:
	  if (string (opt->longname_str0_) == "pdf"
	      || string (opt->longname_str0_) == "png"
	      || string (opt->longname_str0_) == "ps")
	    add_output_format (opt->longname_str0_);
	  else if (string (opt->longname_str0_) == "relocate")
	    relocate_binary = true;
	  break;

	case 'd':
	  {
	    string arg (option_parser->optional_argument_str0_);
	    ssize eq = arg.find ('=');

	    string key = arg;
	    string val = "#t";

	    if (eq != NPOS)
	      {
		key = arg.substr (0, eq);
		val = arg.substr (eq + 1, arg.length () - 1);
	      }

	    init_scheme_variables_global
	      += "(cons \'" + key + " '" + val + ")\n";
	  }
	  break;

	case 'v':
	  notice ();
	  exit (0);
	  break;
	case 'o':
	  {
	    string s = option_parser->optional_argument_str0_;
	    File_name file_name (s);
	    output_name_global = file_name.to_string ();
	  }
	  break;
	case 'j':
	  jail_spec = option_parser->optional_argument_str0_;
	  break;

	case 'e':
	  init_scheme_code_global
            += option_parser->optional_argument_str0_ + string (" ");
	  break;
	case 'w':
	  warranty ();
	  exit (0);
	  break;

	case 'f':
	  {
	    vector<string> components
	      = string_split (option_parser->optional_argument_str0_, ',');
	    for (vsize i = 0; i < components.size (); i++)
	      add_output_format (components[i]);
	  }
	  break;

	case 'H':
	  dump_header_fieldnames_global
	    .push_back (option_parser->optional_argument_str0_);
	  break;
	case 'I':
	  global_path.append (option_parser->optional_argument_str0_);
	  break;
	case 'i':
	  init_name_global = option_parser->optional_argument_str0_;
	  break;
	case 'h':
	  show_help = true;
	  break;
	case 'V':
	  be_verbose_global = true;
	  break;
	default:
	  programming_error (to_string ("unhandled short option: %c",
					opt->shortname_char_));
	  assert (false);
	  break;
	}
    }

  if (output_format_global == "")
    output_format_global = "pdf";

  if (show_help)
    {
      ly_usage ();
      if (be_verbose_global)
	dir_info (stdout);
      exit (0);
    }
}

void
setup_guile_env ()
{
  char const *yield = getenv ("LILYPOND_GC_YIELD");
  bool overwrite = true;
  if (!yield)
    {
      yield = "65";
      overwrite = false;
    }

  sane_putenv ("GUILE_MIN_YIELD_1", yield, overwrite);
  sane_putenv ("GUILE_MIN_YIELD_2", yield, overwrite);
  sane_putenv ("GUILE_MIN_YIELD_MALLOC", yield, overwrite);


  sane_putenv ("GUILE_INIT_SEGMENT_SIZE_1",
	       "10485760", overwrite);
  sane_putenv ("GUILE_MAX_SEGMENT_SIZE",
	       "104857600", overwrite);
}

vector<string> start_environment_global;

int
main (int argc, char **argv, char **envp)
{
  configure_fpu();

  for (char **p = envp; *p; p++)
    start_environment_global.push_back(*p);

  if (getenv ("LILYPOND_VERBOSE"))
    be_verbose_global = true;

  setup_localisation ();
  parse_argv (argc, argv);
  if (isatty (STDIN_FILENO))
    identify (stderr);

  setup_paths (argv[0]);
  setup_guile_env ();

#if 0
  /* Debugging aid.  */
  try
    {
      scm_boot_guile (argc, argv, main_with_guile, 0);
    }
  catch (exception e)
    {
      error (_f ("exception caught: %s", e.what ()));
    };
#else
  scm_boot_guile (argc, argv, main_with_guile, 0);
#endif

  /* Only reachable if GUILE exits.  That is an error.  */
  return 1;
}
