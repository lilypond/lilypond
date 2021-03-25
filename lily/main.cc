/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "lily-guile.hh"

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
#ifdef __MINGW32__
#include <windows.h>
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
#include "warn.hh"
#include "lily-imports.hh"

#if GS_API
#include <ghostscript/iapi.h>
#endif

#include <cassert>
#include <cerrno>
#include <clocale>
#include <cstdio>
#include <cstring>

/*
 * Global options that can be overridden through command line.
 * Most variables are defined in file `global-vars.cc'.
 */

/* Provide URI links to the original file */
bool point_and_click_global = true;

/*
 * File globals.
 */

static char const *AUTHORS
  = "  Han-Wen Nienhuys <hanwen@xs4all.nl>\n"
    "  Jan Nieuwenhuizen <janneke@gnu.org>\n";

static char const *PROGRAM_NAME = "lilypond";
static char const *PROGRAM_URL = "https://lilypond.org";

static char const *NOTICE
  = _i ("This program is free software.  It is covered by the GNU General Public\n"
        "License and you are welcome to change it and/or distribute copies of it\n"
        "under certain conditions.  Invoke as `%s --warranty' for more\n"
        "information.\n");

static char const *WARRANTY
  = _i ("    This program is free software; you can redistribute it and/or\n"
        "modify it under the terms of the GNU General Public License as \n"
        "published by the Free Software Foundation, either version 3 of\n"
        "the License, or (at your option) any later version.\n"
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

/* The jail specification: USER, GROUP, JAIL, DIR. */
std::string jail_spec;

/*  The option parser */
static Getopt_long *option_parser = 0;

/* Internationalisation kludge in two steps:
 * use _i () to get entry in POT file
 * call gettext () explicitly for actual "translation"  */

/*
  Data to be used to display when
  -h command line option is detected.
*/
static Long_option_init options_static[]
=
{
  {
    _i ("FORMATs"), "formats", 'f',
    _i ("dump FORMAT,...  Also as separate options:")
  },
  {
    0, "pdf", 0,
    _i ("generate PDF files (default)")
  },
  {
    0, "svg", 0,
    _i ("generate SVG files ")
  },
  {
    0, "png", 0,
    _i ("generate PNG files ")
  },
  {
    0, "ps", 0,
    _i ("generate PostScript files")
  },
  {
    0, "eps", 'E',
    _i ("generate Encapsulated PostScript files")
  },
  {
    _i ("KEY"), "pspdfopt", 'O',
    _i ("set ps/pdf optimization to KEY, which is either\n"
        "'size' (default), 'TeX', or 'TeX-GS'")
  },
  {
    _i ("SYM[=VAL]"), "define-default", 'd',
    _i ("set Scheme option SYM to VAL (default: #t);\n"
        "use -dhelp for help")
  },
  {
    _i ("EXPR"), "evaluate", 'e',
    _i ("evaluate scheme code")
  },
  /* Bug in option parser: --output =foe is taken as an abbreviation
     for --output-format.  */
  {
    0, "help", 'h',
    _i ("show this help and exit")
  },
  {
    _i ("FIELD"), "header", 'H',
    _i ("dump \\header field FIELD to file\n"
        "named BASENAME.FIELD")
  },
  {
    _i ("DIR"), "include", 'I',
    _i ("append DIR to search path")
  },
  {
    _i ("FILE"), "init", 'i',
    _i ("use FILE as init file")
  },
#if HAVE_CHROOT
  {
    _i ("USER,GROUP,JAIL,DIR"), "jail", 'j',
    _i ("chroot to JAIL, become USER:GROUP\n"
        "and cd into DIR")
  },
#endif
  {
    _i ("LOGLEVEL"), "loglevel", 'l',
    _i ("print log messages according to LOGLEVEL,\n"
        "which is either NONE, ERROR, WARNING,\n"
        "BASIC, PROGRESS, INFO (default), or DEBUG")
  },
  {
    _i ("FILE"), "output", 'o',
    _i ("write output to FILE (suffix will be added)\n"
        "or to FOLDER, in which case the file name\n"
        "will be taken from the input file.")
  },
  {
    0, "relocate", 0,
    _i ("(ignored)")
  },
  {
    0, "silent", 's',
    _i ("no progress, only error messages\n"
        "(equivalent to --loglevel=ERROR)")
  },
  {
    0, "version", 'v',
    _i ("show version number and exit")
  },
  {
    0, "verbose", 'V',
    _i ("be verbose (equivalent to --loglevel=DEBUG)")
  },
  {
    0, "warranty", 'w',
    _i ("show warranty and copyright")
  },
  {0, 0, 0, 0}
};

using std::map;
using std::string;
using std::vector;

static void
env_var_info (FILE *out, char const *key)
/*
 * Retrieve value of an OS environment variable.
 * Parameter:
 *  key, the name of an environment variable.
 */
{
  if (char const *value = getenv (key))
    fprintf (out, "%s=\"%s\"\n", key, value);
}

static void
dir_info (FILE *out)
/*
 * Print out information re directories being used by LilyPond
 * for this session.
 */
{
  fputs ("\n", out);
  env_var_info (out, "LILYPOND_DATADIR");
  env_var_info (out, "LILYPOND_LOCALEDIR");
  env_var_info (out, "LILYPOND_RELOCDIR");

  fputs (_f ("\n"
             "Effective prefix: '%s'\n",
             lilypond_datadir).c_str (), out);

  env_var_info (out, "FONTCONFIG_FILE");
  env_var_info (out, "FONTCONFIG_PATH");
  env_var_info (out, "GS_FONTPATH");
  env_var_info (out, "GS_LIB");
  env_var_info (out, "GUILE_LOAD_PATH");
  env_var_info (out, "PANGO_RC_FILE");
  env_var_info (out, "PANGO_PREFIX");
  env_var_info (out, "PATH");
  fputs ("\n", out);
}

static void
copyright ()
/*
 * Print out LilyPond copyright info.
 */
{
  /* Do not update the copyright years here, run `make grand-replace'  */
  printf ("%s", (_f ("Copyright (c) %s by\n%s  and others.", "1996--2021",
                     AUTHORS).c_str ()));
  printf ("\n");
}

static void
identify (FILE *out)
/*
 * Print out LilyPond version string.
 */
{
  fputs (gnu_lilypond_version_string ().c_str (), out);
  fputs ("\n", out);
}

static void
/*
 * Print copyright and program name
 */
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
/*
 * ly_usage: Routine to output standard information when LilyPond is run without a
 * source file to compile.
 * Also callable as ly:usage from Scheme.
 */
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
  printf ("%s", (_f ("You found a bug? Please read %s",
                     "https://lilypond.org/bug-reports.html")
                 .c_str ()));
  printf ("\n");
  printf ("\n");
  return SCM_UNSPECIFIED;
}

static void
warranty ()
/*
 * Prints out LilyPond warranty information
 */
{
  identify (stdout);
  printf ("\n");
  copyright ();
  printf ("\n");
  printf ("%s", (_ (WARRANTY).c_str ()));

#if GS_API
  printf ("\n");
  printf ("%s", (_ ("linked against Ghostscript:").c_str ()));
  printf ("\n");

  void *gs_inst = NULL;
  gsapi_new_instance (&gs_inst, NULL);
  gsapi_set_arg_encoding (gs_inst, GS_ARG_ENCODING_UTF8);

  // gsapi_init_with_args wants modifiable strings, so create local variables
  // with copies of the content.
  // (The first argument is the "program name" and is ignored.)
  char gs[] = "gs";
  char nodisplay[] = "-dNODISPLAY";
  char *argv[] = { gs, nodisplay };
  gsapi_init_with_args (gs_inst, 2, argv);
  gsapi_exit (gs_inst);
  gsapi_delete_instance (gs_inst);
#endif
}

static void
prepend_scheme_list (const string &dir, const string &scmlist)
/*
 *  Inserts an item at the front of a Scheme list, e.g. %load-path
 *  Parameters:
 *    dir:     The directory to add to the front of the list
 *    scmlist: The Scheme list onto which to prepend the directory
 */
{
  SCM var = scm_c_lookup (scmlist.c_str ());
  scm_variable_set_x (var, scm_cons (scm_from_locale_string (dir.c_str ()),
                                     scm_variable_ref (var)));
  /*  string setcmd =
             "(set! " + scmlist + " (cons \"" + dir + "\" " + scmlist +"))";
             scm_c_eval_string (setcmd.c_str());*/
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
  if (passwd *passwd = getpwnam (components[USER_NAME].c_str ()))
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
  if (group *group = getgrnam (components[GROUP_NAME].c_str ()))
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
/*
 * main-with-guile is invoked as a callback via scm_boot_guile from
 * main.
 * scm_boot_guile will have passed its data, argc and argv parameters
 * to main_with_guile.
 */
{
  /* Engravers use lily.scm contents, so we need to make Guile find it.
     Prepend onto GUILE %load-path.
      %load-path is the symbol Guile searches for .scm files
      %load-compiled-path is the symbol Guile V2 searches for .go files
   */
  string scm_pct_load_path = "%load-path";
  string scm_pct_load_compiled_path = "%load-compiled-path";

  prepend_scheme_list (lilypond_datadir, scm_pct_load_path);
  prepend_scheme_list (lilypond_datadir + "/scm", scm_pct_load_path);

#if 0
  /* this code is dead, but keeping this around until we sort the
     guile2/3 situation. */
  /*
    Just as ughy - prepend "/scm/out" onto GUILE V2+ %load-compiled-path
    and set %compile-fallback-path to our scm/out directory
  */
  /*
    %load-compiled-path is the symbol Guile V2 searches for .go files
  */
  prepend_scheme_list (lilypond_datadir + "/scm/out",
                       scm_pct_load_compiled_path);
  /*
    %compile-fallback-path is the guile cache root for auto-compiled files
  */

  string scm_pct_fallback_path = "%compile-fallback-path";
  string ly_scm_go_dir = lilypond_datadir + "/scm/out";
  //string scm_pct_set_fallback = "(set! " + scm_pct_fallback_path +
  //  " \"" + lilypond_datadir + "/scm/out\")";
  //scm_c_eval_string (scm_pct_set_fallback.c_str() );
  scm_primitive_eval
  (scm_list_3 (scm_from_latin1_symbol ("set!"),
               scm_from_latin1_symbol ("%compile-fallback-path"),
               scm_from_locale_string (ly_scm_go_dir.c_str ())));
#endif

  if (is_loglevel (LOG_DEBUG))
    dir_info (stderr);

  init_scheme_variables_global = "(" + init_scheme_variables_global + ")";
  init_scheme_code_global = "(begin " + init_scheme_code_global + ")";

  ly_c_init_guile ();
  call_constructors ();
  init_fontconfig ();

  init_freetype ();

  /*
     We accept multiple independent music files on the command line to
     reduce compile time when processing lots of small files.
     This way we don't have to start the Guile/Scheme interpreter more than once, as
     starting the GUILE engine is very time consuming.
  */

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
  /*
    Now execute the Scheme entry-point declared in
    lily.scm (lilypond-main)
  */
  // These commands moved to lily_guile_v2.scm
  // SCM rep_mod = scm_c_resolve_module ("system repl repl");
  // scm_c_use_module ("system repl repl");
  // SCM err_handling_mod = scm_c_resolve_module ("system repl error-handling");
  // SCM call_with_error_handling = scm_c_module_lookup (err_handling_mod, "call-with-error-handling");
  // SCM result = scm_call_1 (
  //                       scm_variable_ref (call_with_error_handling),
  //                       scm_call_1 (ly_lily_module_constant ("lilypond-main"), files));

  Lily::lilypond_main (files);

  /* Unreachable.  */
  exit (0);
}

static void
setup_localisation ()
/*
 *  Set up local language text locale (if available from configure)
 *  Implicit inputs:
 *  HAVE_GETTEXT: Internationalization available for a local language.
 */
{
#if HAVE_GETTEXT
  /* Enable locales */
#if !defined (__MINGW32__) || defined (_UCRT)
  setlocale (LC_ALL, "");
#else
  // Workaround for MinGW UTF-8 locale settings issue:
  // `setlocale (LC_ALL, "")` in msvcrt.dll sets user-defined ANSI code page
  // (i.e. 437 for English, 932 for Japanse, etc.)
  // even if process code page is UTF-8 (i.e. 65001 for all languages).
  // With this setting, outputting UTF-8 string becomes garbled.
  // So we get the process code page with `GetACP ()` and,
  // if it is UTF-8, explicitly set locale's code page to UTF-8 (i.e. 65001).
  // If Universal CRT (UCRT, newer than msvcrt.dll) is used
  // (i.e. `_UCRT' is defined), no such workaround is needed.
  if (GetACP () == CP_UTF8)
    setlocale (LC_ALL, ".65001");
  else
    setlocale (LC_ALL, "");
#endif

  /* FIXME: check if this is still true.
     Disable localisation of float values. */
  setlocale (LC_NUMERIC, "C");

#if GUILEV2
  // In order not to have this porting aid backfire to GUILE1 usage,
  // this is only compiled in the GUILEV2 version.  It should
  // eventually be replaced with proper multibyte communication with
  // GUILE2, but in the mean time it seems that this is the least
  // invasive path to get comparable results between the
  // not-really-multibyte-supporting GUILE1 and GUILE2

  /* Disable character sets */
  setlocale (LC_CTYPE, "C");
  /* But our text domain is in UTF-8 */
  bind_textdomain_codeset ("lilypond", "UTF-8");
#endif

  // we temporarily use the compile-time value for the locale
  // until we get the final directory location
  bindtextdomain ("lilypond", LOCALEDIR);
  textdomain ("lilypond");
#endif
}

static void
add_output_format (const string &format)
/*
 * Capture information internally from command-line options
 * re output format.
 *
 */
{
  if (output_format_global != "")
    output_format_global += ",";
  output_format_global += format;
}

static void
parse_argv (int argc, char **argv)
/*
 *  Parse command-line options
 *  also, if -h (help), -v (version) or  -w (warranty) is detected,
 *  output the usage information and exit.
 */
{
  bool show_help = false;
  option_parser = new Getopt_long (argc, argv, options_static);
  while (Long_option_init const *opt = (*option_parser) ())
    {
      switch (opt->shortname_char_)
        {
        case 'f':
          {
            vector<string> components
              = string_split (option_parser->optional_argument_str0_, ',');
            bool wants_svg = 0;
            for (vsize i = 0; i < components.size (); i++)
              {
                string format = (components[i]);
                if (format == "svg")
                  {
                    wants_svg = 1;
                    init_scheme_variables_global += "(backend . svg)\n";
                  }
                if ((i > 0) && (wants_svg))
                  {
                    warning (_ ("SVG backend requested; other formats will be ignored."));
                    wants_svg = 0;
                  }
                add_output_format (format);
              }
          }
          break;

        case 0:
          if (string (opt->longname_str0_) == "pdf"
              || string (opt->longname_str0_) == "png"
              || string (opt->longname_str0_) == "ps")
            add_output_format (opt->longname_str0_);
          else if (string (opt->longname_str0_) == "svg")
            init_scheme_variables_global += "(backend . svg)\n";
          else if (string (opt->longname_str0_) == "relocate")
            warning (_ ("The --relocate option is no longer relevant."));
          break;

        case 'E':
          add_output_format ("ps");
          init_scheme_variables_global
          += "(backend . eps)\n(aux-files . #f)\n";
          break;

        case 'O':
          {
            string arg (option_parser->optional_argument_str0_);
            if (arg == "size")
              init_scheme_variables_global
              += "(music-font-encodings . #f)\n"
                 "(gs-never-embed-fonts . #f)\n";
            else if (arg == "TeX-GS")
              init_scheme_variables_global
              += "(music-font-encodings . #t)\n"
                 "(gs-never-embed-fonts . #t)\n";
            else if (arg == "TeX")
              init_scheme_variables_global
              += "(music-font-encodings . #t)\n"
                 "(gs-never-embed-fonts . #f)\n";
            else
              programming_error ("Ignoring unknown optimization key");
          }
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
            += "(" + key + " . " + val + ")\n";
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
          set_loglevel (LOGLEVEL_DEBUG);
          break;
        case 's':
          set_loglevel (LOGLEVEL_ERROR);
          break;
        case 'l':
          set_loglevel (option_parser->optional_argument_str0_);
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
      if (is_loglevel (LOG_DEBUG))
        dir_info (stdout);
      exit (0);
    }
}

/*
  T1686 Add two new routines called by setup_guile_env
*/

void
setup_guile_gc_env ()
/*
 * Set up environment variables relevant to the
 * Garbage Collector
 */
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

#if (GUILEV2)

void
setup_guile_v2_env ()
/*
 * Set up environment variables relevant to compiling
 * Scheme files for Guile V2.
 */
{
  sane_putenv ("GUILE_AUTO_COMPILE", "0", false); // disable auto-compile
  sane_putenv ("GUILE_WARN_DEPRECATED", "detailed",
               "true"); // set Guile to info re deprecation
  /*
        Set root for Guile %compile-fallback to
        Lilypond root for its data.
      */
  sane_putenv ("XDG_CACHE_HOME", lilypond_datadir, true);

  // This reduces the GC overhead during parsing and
  // initialization. To see if this is a good value, run #(display
  // (gc-stats)) just before \maininput in init.ly, and check that
  // it's roughly this value.
  sane_putenv ("GC_INITIAL_HEAP_SIZE", "40M", false);

  /*
    Empirically, multithreaded GC doesn't change wall time. It just
    adds another thread that burns 30% of the time.

    David K mentions: "I think that this may be due to both/either our
    use of mark hooks and of finalisers for calling destructors.
    Either may cause serialisation.  Another serialisation is because
    Guile itself switches BGC to Java mode where finalised objects can
    no longer be marked (or something like that: the exact semantics I
    do not remember).  And of course the C++ free store still has to
    do its full job.
  */
  sane_putenv ("GC_NPROCS", "1", false);

  // Use less CPU for GC, at the expense of memory.
  sane_putenv ("GC_FREE_SPACE_DIVISOR", "1", false);
}
#endif

void
setup_guile_env ()
/*
 * Set up environment variables relevant to Scheme
 */
{
#if (GUILEV2)
  setup_guile_v2_env ();  // configure Guile V2 behaviour
#else
  setup_guile_gc_env ();  // configure garbage collector
#endif
}

//vector<string> start_environment_global;

int
main (int argc, char **argv, char **envp)
/*
 * Main entry-point for LilyPond executable image
 * Parameters:
 * argc:   Count of arguments on the command line
 * argv:   Vector of string arguments on command line
 * envp:   Point to vector of OS environment variables
 */
{
  /*
    Process environment variables
  */
  for (char **p = envp; *p; p++)
    start_environment_global.push_back (*p);
  /*
    Handle old-style environment equivalent to
    old-style -V or --verbose command arguments.
    Set it to the equivalent for --loglevel-DEBUG
   */
  if (getenv ("LILYPOND_VERBOSE"))
    set_loglevel (LOGLEVEL_DEBUG);
  if (getenv ("LILYPOND_LOGLEVEL"))
    set_loglevel (getenv ("LILYPOND_LOGLEVEL"));

  setup_localisation ();
  parse_argv (argc, argv);
  if (isatty (STDIN_FILENO) && (is_loglevel (LOG_BASIC)))
    identify (stderr);

  setup_paths (argv[0]);
  setup_guile_env ();  // set up environment variables to pass into Guile API

#if !GS_API
  // Let Guile know whether the Ghostscript API is not available.
  init_scheme_variables_global += "(gs-api . #f)\n";
#endif

  /*
   * Start up Guile API using main_with_guile as a callback.
   */
  scm_boot_guile (argc, argv, main_with_guile, 0);

  /* Only reachable if GUILE exits.  That is an error.  */
  return 1;
}
