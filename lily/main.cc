/*
  main.cc -- implement main () entrypoint.

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "main.hh"

#include <cassert>
#include <clocale>
#include <cstring>
#include <cerrno>
using namespace std;

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

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
#include "lily-guile.hh"
#include "lily-version.hh"
#include "misc.hh"
#include "output-def.hh"
#include "string-convert.hh"
#include "version.hh"
#include "warn.hh"
#include "program-option.hh"

/*
 * Global options that can be overridden through command line.
 */

/* Names of header fields to be dumped to a separate file. */
Array<String> dump_header_fieldnames_global;

/* Name of initialisation file. */
String init_name_global;

/* Selected output backend
   One of (gnome, ps [default], eps, scm, svg, tex, texstr)") */
String output_backend_global = "ps";
/* Output formats to generate.  */
String output_format_global = "";

bool is_pango_format_global;
bool is_TeX_format_global;

/* Current output name. */
String output_name_global;

/* Run in safe mode? */
bool be_safe_global = false;

/* Provide URI links to the original file */
bool point_and_click_global = true;

/* Verbose progress indication? */
bool be_verbose_global = false;

/* Scheme code to execute before parsing, after .scm init.
   This is where -e arguments are appended to.  */
String init_scheme_code_string;
String init_scheme_variables;

/* Generate preview of first system.  */
bool make_preview = false;

/* Generate printed output.  */
bool make_print = true;

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
      "    You should have received a copy (refer to the file COPYING) of the\n"
      "GNU General Public License along with this program; if not, write to\n"
      "the Free Software Foundation, Inc., 59 Temple Place - Suite 330,\n"
      "Boston, MA 02111-1307, USA.\n");

/* Where the init files live.  Typically:
   LILYPOND_DATADIR = /usr/share/lilypond
*/
String prefix_directory;

/* The jail specification: USER,GROUP,JAIL,DIR. */
String jail_spec;

/*  The option parser */
static Getopt_long *option_parser = 0;

/* Internationalisation kludge in two steps:
 * use _i () to get entry in POT file
 * call gettext () explicitely for actual "translation"  */

static Long_option_init options_static[]
= {
  {_i ("BACK"), "backend", 'b', _i ("use backend BACK (gnome, ps,eps,\nscm, svg, tex, texstr)\ndefault: PS")},

  {_i ("SYM=VAL"), "define-default", 'd',
   _i ("set a Scheme program option. Uses #t if VAL is not specified\n"
       "Try -dhelp for help.")},

  {_i ("EXPR"), "evaluate", 'e', _i ("evaluate scheme code")},
  /* Bug in option parser: --output =foe is taken as an abbreviation
     for --output-format.  */
  {_i ("FORMATs"), "formats", 'f', _i ("dump FORMAT,...  Also as separate options:")},
  {0, "dvi", 0, _i ("generate DVI (tex backend only)")},
  {0, "pdf", 0, _i ("generate PDF (default)")},
  {0, "png", 0, _i ("generate PNG")},
  {0, "ps", 0, _i ("generate PostScript")},
  {0, "tex", 0, _i ("generate TeX (tex backend only)")},
  {0, "help", 'h',  _i ("print this help")},
  {_i ("FIELD"), "header", 'H',  _i ("dump a header field to file BASENAME.FIELD")},
  {_i ("DIR"), "include", 'I',  _i ("add DIR to search path")},
  {_i ("FILE"), "init", 'i',  _i ("use FILE as init file")},
  {_i ("FILE"), "output", 'o',  _i ("write output to FILE (suffix will be added)")},
#if HAVE_CHROOT
  {_i ("USER,GROUP,JAIL,DIR"), "jail", 'j', _i ("chroot to JAIL, become USER:GROUP\n"
						"and cd into DIR")},
#endif
  {0, "no-print", 0, _i ("do not generate printed output")},
  {0, "preview", 'p',  _i ("generate a preview of the first system")},
  {0, "safe-mode", 's',  _i ("disallow unsafe Scheme and PostScript operations")},
  {0, "version", 'v',  _i ("print version number")},
  {0, "verbose", 'V', _i ("be verbose")},
  {0, "warranty", 'w',  _i ("show warranty and copyright")},
  {0, 0, 0, 0}
};

#define LILYPOND_DATADIR PACKAGE_DATADIR "/" TOPLEVEL_VERSION

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
  fprintf (out, "LOCALEDIR=\"%s\"\n", LOCALEDIR);

  fprintf (out, "\nEffective prefix: \"%s\"\n", prefix_directory.to_str0 ());

#if ARGV0_RELOCATION
  env_var_info (out, "FONTCONFIG_FILE");
  env_var_info (out, "FONTCONFIG_PATH");
  env_var_info (out, "GS_FONTPATH");
  env_var_info (out, "GS_LIB");
  env_var_info (out, "GUILE_LOAD_PATH");
  env_var_info (out, "PANGO_RC_FILE");
  env_var_info (out, "PATH");
#endif
}

static void
copyright ()
{
  printf (_f ("Copyright (c) %s by\n%s  and others.",
	      "1996--2005",
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
  puts (_f (NOTICE, PROGRAM_NAME).to_str0 ());
  printf ("\n");
  copyright ();
}

LY_DEFINE (ly_usage, "ly:usage",
	   0, 0, 0, (),
	   "Print usage message.")
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
  printf (_f ("Report bugs via %s",
	      "http://post.gmane.org/post.php?group=gmane.comp.gnu.lilypond.bugs"
	      ).to_str0 ());
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
  printf (_ (WARRANTY).to_str0 ());
}

static int
sane_putenv (char const *key, String value, bool overwrite = true)
{
  if (overwrite || !getenv (key))
    {
      String combine = String (key) + "=" + value;
      char *s = strdup (combine.to_str0 ());
      return putenv (s);
    }
  return -1;
}

#if ARGV0_RELOCATION
static int
set_env_file (char const *key, String value)
{
  if (is_file (value))
    return sane_putenv (key, value, false);
  else if (be_verbose_global)
    warning (_f ("no such file: %s", value));
  return -1;
}

static int
prepend_env_path (char const *key, String value)
{
  if (is_dir (value))
    {
      if (char const *cur = getenv (key))
	value += to_string (PATHSEP) + cur;
      return sane_putenv (key, value.to_str0 ());
    }
  else if (be_verbose_global)
    warning (_f ("no such directory: %s", value));
  return -1;
}

String
dir_name (String const file_name)
{
  String s = file_name;
  s.substitute ('\\', '/');
  s = s.left_string (s.index_last ('/'));
  return s;
}
#endif

#ifdef __MINGW32__
#include <winbase.h>
#endif

static void
setup_paths (char const *argv0)
{
  prefix_directory = LILYPOND_DATADIR;

#if ARGV0_RELOCATION

  if (getenv ("LILYPOND_VERBOSE"))
    be_verbose_global = true;

  /* Find absolute ARGV0 name, using PATH.  */
  File_path path;
  path.parse_path (getenv ("PATH"));

#if defined (__CYGWIN__) || defined (__MINGW32__)
  String s = argv0;
  s.substitute ('\\', '/');
  argv0 = s.to_str0 ();
#endif /* __CYGWIN__ || __MINGW32__ */

#ifndef __MINGW32__
  String argv0_abs = path.find (argv0);
#else /* __MINGW32__ */
  char const *ext[] = {"exe", "", 0 };
  String argv0_abs = path.find (argv0, ext);
#endif /* __MINGW32__ */

  if (argv0_abs.is_empty ())
    {
      File_name name (argv0);
      /* If NAME contains slashes and its DIR is not absolute, it can
	 only be referenced from CWD.  */
      if (name.to_string ().index ('/') >= 0 && name.dir_[0] != '/')
	{
	  char cwd[PATH_MAX];
	  getcwd (cwd, PATH_MAX);
	  argv0_abs = String (cwd) + "/" + argv0;
	}
      else
	programming_error ("can't find absolute argv0");
    }
    
  String bindir = dir_name (argv0_abs);
  String argv0_prefix = dir_name (bindir);
  if (argv0_prefix != dir_name (dir_name (dir_name (prefix_directory))))
    {
      if (be_verbose_global)
	warning (_f ("argv0 relocation: prefix=%s, argv0=%s, argv0_prefix=%s",
		     prefix_directory,
		     argv0,
		     argv0_prefix.to_str0 ()));
      String datadir = argv0_prefix + "/share";
      String libdir = argv0_prefix + "/lib";
      String localedir = datadir + "/locale";
      String sysconfdir = argv0_prefix + "/etc";
      String argv0_lilypond_datadir = datadir + "/lilypond/" TOPLEVEL_VERSION;

      if (is_dir (argv0_lilypond_datadir))
	prefix_directory = argv0_lilypond_datadir;

#if HAVE_GETTEXT
      if (is_dir (localedir))
	bindtextdomain ("lilypond", localedir.to_str0 ());
#endif

      set_env_file ("FONTCONFIG_FILE", sysconfdir + "/fonts/fonts.conf");
#ifdef __MINGW32__
      char font_dir[PATH_MAX];
      ExpandEnvironmentStrings ("%windir%/fonts", font_dir, sizeof (font_dir));
      prepend_env_path ("GS_FONTPATH", font_dir);
#endif

      /* FIXME: *cough* 8.15 *cough* */
      prepend_env_path ("GS_FONTPATH", datadir + "/ghostscript/8.15/fonts");
      prepend_env_path ("GS_LIB", datadir + "/ghostscript/8.15/Resource");
      prepend_env_path ("GS_LIB", datadir + "/ghostscript/8.15/lib");

      prepend_env_path ("GS_FONTPATH", datadir + "/gs/fonts");
      prepend_env_path ("GS_LIB", datadir + "/gs/Resource");
      prepend_env_path ("GS_LIB", datadir + "/gs/lib");

      prepend_env_path ("GUILE_LOAD_PATH", datadir
			+ to_string ("/guile/%d.%d",
				     SCM_MAJOR_VERSION, SCM_MINOR_VERSION));
      set_env_file ("PANGO_RC_FILE", sysconfdir + "/pango/pangorc");
      prepend_env_path ("PATH", bindir);
    }
#else
  (void) argv0;
#endif /* ARGV0_RELOCATION */

  /* FIXME: use LILYPOND_DATADIR.  */
  if (char const *env = getenv ("LILYPONDPREFIX"))
    {
#ifdef __MINGW32__
      /* Normalize file name.  */
      env = File_name (env).to_string ().get_copy_str0 ();
#endif
      prefix_directory = env;
    }

  global_path.append ("");


  /*
    When running from build dir, a full LILYPOND_PREFIX is set-up at

        $(OUTBASE)/share/lilypond/TOPLEVEL_VERSION

     This historical hack will allow the shorthand

        LILYPONDPREFIX=out lily/out/lilypond ...

  */
  
  struct stat statbuf;
  String build_prefix = prefix_directory + "/share/lilypond/" TOPLEVEL_VERSION;
  if (stat (build_prefix.to_str0 (), &statbuf) == 0)
    prefix_directory = build_prefix;

  
  /* Adding mf/out make lilypond unchanged source directory, when setting
     LILYPONDPREFIX to lilypond-x.y.z */
  char *suffixes[] = {"ly", "ps", "scm", 0 };

  
  Array<String> dirs;
  for (char **s = suffixes; *s; s++)
    {
      String path = prefix_directory + to_string ('/') + String (*s);
      dirs.push (path);
    }


  dirs.push (prefix_directory + "/fonts/otf/");
  dirs.push (prefix_directory + "/fonts/type1/");
  dirs.push (prefix_directory + "/fonts/svg/");
  
  for (int i = 0; i < dirs.size (); i++)
    global_path.prepend (dirs[i]);
}

static void
prepend_load_path (String dir)
{
  String s = "(set! %load-path (cons \"" + dir + "\" %load-path))";
  scm_c_eval_string (s.to_str0 ());
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

  Array<String> components = String_convert::split (jail_spec, ',');
  if (components.size () != JAIL_MAX)
    {
      error (_f ("expected %d arguments with jail, found: %d", JAIL_MAX,
		 components.size ()));
      exit (2);
    }

  /* Hmm.  */
  errno = 0;

  int uid;
  if (passwd * passwd = getpwnam (components[USER_NAME].to_str0 ()))
    uid = passwd->pw_uid;
  else
    {
      if (errno == 0)
	error (_f ("no such user: %s", components[USER_NAME]));
      else
	error (_f ("can't get user id from user name: %s: %s",
		   components[USER_NAME],
		   strerror (errno)));
      exit (3);
    }

  /* Hmm.  */
  errno = 0;

  int gid;
  if (group * group = getgrnam (components[GROUP_NAME].to_str0 ()))
    gid = group->gr_gid;
  else
    {
      if (errno == 0)
	error (_f ("no such group: %s", components[GROUP_NAME]));
      else
	error (_f ("can't get group id from group name: %s: %s",
		   components[GROUP_NAME],
		   strerror (errno)));
      exit (3);
    }

  if (chroot (components[JAIL].to_str0 ()))
    {
      error (_f ("can't chroot to: %s: %s", components[JAIL],
		 strerror (errno)));
      exit (3);
    }

  if (setgid (gid))
    {
      error (_f ("can't change group id to: %d: %s", gid, strerror (errno)));
      exit (3);
    }

  if (setuid (uid))
    {
      error (_f ("can't change user id to: %d: %s", uid, strerror (errno)));
      exit (3);
    }

  if (chdir (components[DIR].to_str0 ()))
    {
      error (_f ("can't change working directory to: %s: %s", components[DIR],
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

  prepend_load_path (prefix_directory);
  prepend_load_path (prefix_directory + "/scm");

  if (be_verbose_global)
    dir_info (stderr);
  is_TeX_format_global = (output_backend_global == "tex"
			  || output_backend_global == "texstr");

  is_pango_format_global = !is_TeX_format_global;

  ly_c_init_guile ();
  call_constructors ();
  ly_set_option (ly_symbol2scm ("verbose"), scm_from_bool (be_verbose_global));

  init_global_tweak_registry ();
  init_fontconfig ();

  init_freetype ();

  all_fonts_global = new All_font_metrics (global_path.to_string ());

  if (!init_scheme_variables.is_empty ()
      || !init_scheme_code_string.is_empty ())
    {
      init_scheme_variables = "(map (lambda (x) (ly:set-option (car x) (cdr x))) (list "
	+ init_scheme_variables + "))";

      init_scheme_code_string
	+= "(begin #t "
	+ init_scheme_variables
	+ init_scheme_code_string
	+ ")";

      char const *str0 = init_scheme_code_string.to_str0 ();

      if (be_verbose_global)
	progress_indication (_f ("Evaluating %s", str0));
      scm_c_eval_string ((char *) str0);
    }

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

#if HAVE_CHROOT
  if (!jail_spec.is_empty ())
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
     Disable localisation of float values.  This breaks TeX output.  */
  setlocale (LC_NUMERIC, "C");

  String localedir = LOCALEDIR;
  if (char const *env = getenv ("LILYPOND_LOCALEDIR"))
    localedir = env;

  bindtextdomain ("lilypond", localedir.to_str0 ());
  textdomain ("lilypond");
#endif
}

static void
add_output_format (String format)
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
	  if (String (opt->longname_str0_) == "dvi"
	      || String (opt->longname_str0_) == "pdf"
	      || String (opt->longname_str0_) == "png"
	      || String (opt->longname_str0_) == "ps"
	      || String (opt->longname_str0_) == "tex")
	    add_output_format (opt->longname_str0_);
	  else if (String (opt->longname_str0_) == "preview")
	    make_preview = true;
	  else if (String (opt->longname_str0_) == "no-pages")
	    make_print = false;
	  break;

	case 'd':
	  {
	    String arg (option_parser->optional_argument_str0_);
	    int eq = arg.index ('=');

	    String key = arg;
	    String val = "#t";

	    if (eq >= 0)
	      {
		key = arg.left_string (eq);
		val = arg.right_string (arg.length () - eq - 1);
	      }

	    init_scheme_variables
	      += "(cons \'" + key + "  " + val + ")\n";
	  }
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
	case 'j':
	  jail_spec = option_parser->optional_argument_str0_;
	  break;
	case 'e':
	  init_scheme_code_string += option_parser->optional_argument_str0_;
	  break;
	case 'w':
	  warranty ();
	  exit (0);
	  break;

	case 'b':
	  output_backend_global = option_parser->optional_argument_str0_;
	  break;

	case 'f':
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
	  show_help = true;
	  break;
	case 'V':
	  be_verbose_global = true;
	  break;
	case 's':
	  be_safe_global = true;
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

  if (output_format_global == "")
    output_format_global = "pdf";

  if (show_help)
    {
      identify (stdout);
      ly_usage ();
      if (be_verbose_global)
	dir_info (stdout);
      exit (0);
    }
}

void
setup_guile_env ()
{
  char *yield = getenv ("LILYPOND_GC_YIELD");
  bool overwrite = true;
  if (!yield)
    {
      yield = "70";
      overwrite = false;
    }

  sane_putenv ("GUILE_MIN_YIELD_1", yield, overwrite);
  sane_putenv ("GUILE_MIN_YIELD_2", yield, overwrite);
  sane_putenv ("GUILE_MIN_YIELD_MALLOC", yield, overwrite);
}

int
main (int argc, char **argv)
{
  setup_localisation ();
  setup_paths (argv[0]);
  setup_guile_env ();
  parse_argv (argc, argv);
  if (isatty (STDIN_FILENO))
    identify (stderr);

  scm_boot_guile (argc, argv, main_with_guile, 0);

  /* Only reachable if GUILE exits.  That is an error.  */
  return 1;
}
