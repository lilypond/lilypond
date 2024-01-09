/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "config.hh"

#include "main.hh"
#include "lily-guile.hh"

#include <unistd.h>
#include <sys/types.h>

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
#include "international.hh"
#include "lily-imports.hh"
#include "misc.hh"
#include "output-def.hh"
#include "program-option.hh"
#include "relocate.hh"
#include "std-vector.hh"
#include "string-convert.hh"
#include "version.hh"
#include "warn.hh"

#if GS_API
#include <ghostscript/iapi.h>
#endif

#include <cassert>
#include <cerrno>
#include <clocale>
#include <cstdio>
#include <cstring>

// Global options that can be overridden through command line.
// Most variables are defined in file `global-vars.cc'.

// Provide URI links to the original file
bool point_and_click_global = true;

// File globals.
static char const *AUTHORS = "  Han-Wen Nienhuys <hanwen@xs4all.nl>\n"
                             "  Jan Nieuwenhuizen <janneke@gnu.org>\n";

static char const *PROGRAM_NAME = "lilypond";
static char const *PROGRAM_URL = "https://lilypond.org";

static char const *NOTICE = _i (
  "This program is free software.  It is covered by the GNU General Public\n"
  "License and you are welcome to change it and/or distribute copies of it\n"
  "under certain conditions.  Invoke as `%s --warranty' for more\n"
  "information.\n");

static char const *WARRANTY
  = _i ("    This program is free software; you can redistribute it and/or\n"
        "modify it under the terms of the GNU General Public License as\n"
        "published by the Free Software Foundation, either version 3 of\n"
        "the License, or (at your option) any later version.\n"
        "\n"
        "    This program is distributed in the hope that it will be useful,\n"
        "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
        "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n"
        "General Public License for more details.\n"
        "\n"
        "    You should have received a copy of the GNU General Public\n"
        "License along with this program.  If not, see\n"
        "https://www.gnu.org/licenses/.\n");

/* The jail specification: USER, GROUP, JAIL, DIR. */
std::string jail_spec;

/*  The option parser */
static Getopt_long *option_parser = 0;

// Data to be used to display when -h command-line option is detected.
static Long_option_init options_static[] = {
  {_i ("FORMATs"), "formats", 'f',
   _i ("dump FORMAT,...  Also as separate options:")},
  {0, "pdf", 0, _i ("generate PDF files (default)")},
  {0, "svg", 0, _i ("generate SVG files ")},
  {0, "png", 0, _i ("generate PNG files ")},
  {0, "ps", 0, _i ("generate PostScript files")},
  {0, "eps", 'E', _i ("generate Encapsulated PostScript files")},
  {_i ("KEY"), "pspdfopt", 'O',
   _i ("set ps/pdf optimization to KEY, which is\n"
       "either 'size' (default), 'TeX', or\n"
       "'TeX-GS'")},
  {_i ("SYM[=VAL]"), "define-default", 'd',
   _i ("set Scheme option SYM to VAL (default:\n"
       "'#t')")},
  {_i ("no-SYM"), "define-default", 'd', _i ("set Scheme option SYM to '#f'")},
  {"help", "define-default", 'd', _i ("show help for Scheme options")},
  {_i ("EXPR"), "evaluate", 'e', _i ("evaluate scheme code")},
  /* Bug in option parser: --output =foe is taken as an abbreviation
     for --output-format.  */
  {0, "help", 'h', _i ("show this help and exit")},
  {_i ("FIELD"), "header", 'H',
   _i ("dump \\header field FIELD to file named\n"
       "BASENAME.FIELD")},
  {_i ("DIR"), "include", 'I', _i ("append DIR to search path")},
  {_i ("FILE"), "init", 'i', _i ("use FILE as init file")},
#if HAVE_CHROOT
  {_i ("USER,GROUP,JAIL,DIR"), "jail", 'j',
   _i ("chroot to JAIL, become USER:GROUP and cd\n"
       "into DIR")},
#endif
  {_i ("LOGLEVEL"), "loglevel", 'l',
   _i ("print log messages according to LOGLEVEL,\n"
       "which is either NONE, ERROR, WARNING,\n"
       "BASIC, PROGRESS, INFO (default), or DEBUG")},
  {_i ("FILE"), "output", 'o',
   _i ("write output to FILE (suffix will be added)\n"
       "or to FOLDER, in which case the file name\n"
       "will be taken from the input file.")},
  {0, "relocate", 0, _i ("(ignored)")},
  {0, "silent", 's',
   _i ("no progress, only error messages\n"
       "(equivalent to --loglevel=ERROR)")},
  {0, "version", 'v', _i ("show version number and exit")},
  {0, "verbose", 'V', _i ("be verbose (equivalent to --loglevel=DEBUG)")},
  {0, "warranty", 'w', _i ("show warranty and copyright")},
  {0, 0, 0, 0}};

// Retrieve value of an OS environment variable.
static void
env_var_info (FILE *out, char const *key)
{
  if (char const *value = getenv (key))
    fprintf (out, "%s=\"%s\"\n", key, value);
}

// Print out information re directories being used by LilyPond for this session.
static void
dir_info (FILE *out)
{
  fputs ("\n", out);
  env_var_info (out, "LILYPOND_DATADIR");
  env_var_info (out, "LILYPOND_LOCALEDIR");
  env_var_info (out, "LILYPOND_RELOCDIR");

  fputs (_f ("\n"
             "Effective prefix: '%s'\n",
             lilypond_datadir)
           .c_str (),
         out);

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

static std::string
gnu_lilypond_version_string ()
{
  // can't use version_string(), because Guile hasn't started yet.
  std::string version = MAJOR_VERSION "." MINOR_VERSION "." PATCH_LEVEL;
  std::string mpl (MY_PATCH_LEVEL);
  if (mpl != "")
    version += "." + mpl;

  std::string guile_version = std::to_string (SCM_MAJOR_VERSION) + "."
                              + std::to_string (SCM_MINOR_VERSION);
  std::string str
    = _f ("%s %s (running Guile %s)", "GNU LilyPond", version, guile_version);
  return str;
}

// Print out LilyPond copyright info
static void
copyright ()
{
  /* Do not update the copyright years here, run `make grand-replace'  */
  printf ("%s",
          (_f ("Copyright (c) %s by\n%s  and others.", "1996--2023", AUTHORS)
             .c_str ()));
  printf ("\n");
}

// Print out LilyPond version string
static void
identify (FILE *out)
{
  fputs (gnu_lilypond_version_string ().c_str (), out);
  fputs ("\n", out);
}

// Print copyright and program name
static void
notice ()
{
  identify (stdout);
  printf ("\n");
  copyright ();
  printf ("\n");
  puts (_f (NOTICE, PROGRAM_NAME).c_str ());
}

LY_DEFINE (ly_usage, "ly:usage", 0, 0, 0, (),
           R"(
Print usage message.
           )")
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

// Prints out LilyPond warranty information
static void
warranty ()
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
  char *argv[] = {gs, nodisplay};
  gsapi_init_with_args (gs_inst, 2, argv);
  gsapi_exit (gs_inst);
  gsapi_delete_instance (gs_inst);
#endif
}

// Inserts an item at the front of a Scheme list, e.g. %load-path
static void
prepend_scheme_list (const std::string &dir, const std::string &scmlist)
{
  SCM var = scm_c_lookup (scmlist.c_str ());
  scm_variable_set_x (var, scm_cons (scm_from_locale_string (dir.c_str ()),
                                     scm_variable_ref (var)));
}

void init_global_tweak_registry ();

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
    USER_NAME,
    GROUP_NAME,
    JAIL,
    DIR,
    JAIL_MAX
  };

  std::vector<std::string> components = string_split (jail_spec, ',');
  if (components.size () != JAIL_MAX)
    {
      error (_f ("expected %d arguments with jail, found: %zu", JAIL_MAX,
                 components.size ()));
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
                   components[USER_NAME], strerror (errno)));
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
                   components[GROUP_NAME], strerror (errno)));
      exit (3);
    }

  if (chroot (components[JAIL].c_str ()))
    {
      error (
        _f ("cannot chroot to: %s: %s", components[JAIL], strerror (errno)));
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

// main-with-guile is invoked as a callback via scm_boot_guile from main.
// scm_boot_guile passes its data, argc and argv parameters to main_with_guile.
static void
main_with_guile (void *, int, char **)
{
  /* Engravers use lily.scm contents, so we need to make Guile find it.
     Prepend onto Guile %load-path.
      %load-path is the symbol Guile searches for .scm files
      %load-compiled-path is the symbol Guile V2 searches for .go files
   */
  std::string scm_pct_load_path = "%load-path";
  prepend_scheme_list (lilypond_datadir + "/scm", scm_pct_load_path);

  std::string scm_pct_load_compiled_path = "%load-compiled-path";
  prepend_scheme_list (lilypond_libdir + "/ccache", scm_pct_load_compiled_path);

  // TODO: consider introducing a -dbytecode-path option.
  scm_variable_set_x (
    scm_c_lookup ("%compile-fallback-path"),
    scm_from_locale_string ((lilypond_datadir + "/guile-bytecode").c_str ()));

  if (is_loglevel (LOG_DEBUG))
    dir_info (stderr);

  init_scheme_code_global = "(begin " + init_scheme_code_global + ")";

  ly_c_init_guile ();

  init_freetype ();

  /*
     We accept multiple independent music files on the command line to
     reduce compile time when processing lots of small files.
     This way we don't have to start the Guile/Scheme interpreter more than once, as
     starting the Guile engine is very time consuming.
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

  Lily::lilypond_main (files);

  /* Unreachable.  */
  exit (0);
}

// Set up local language text locale (if available from configure)
static void
setup_localisation ()
{
#if HAVE_GETTEXT
  /* Enable locales */
#if !defined(__MINGW32__) || defined(_UCRT)
  setlocale (LC_ALL, "");
#else
  // Workaround for MinGW UTF-8 locale settings issue:
  // `setlocale (LC_ALL, "")` in msvcrt.dll sets user-defined ANSI code page
  // (i.e. 437 for English, 932 for Japanese, etc.)
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

  // we temporarily use the compile-time value for the locale
  // until we get the final directory location
  bindtextdomain ("lilypond", LOCALEDIR);
  textdomain ("lilypond");
#endif
}

static void
add_output_format (const std::string &format)
{
  if (std::find (output_formats_global.begin (), output_formats_global.end (),
                 format)
      == output_formats_global.end ())
    output_formats_global.push_back (format);
}

// Parse command-line options
// also, if -h (help), -v (version) or  -w (warranty) is detected,
// output the usage information and exit.
static void
parse_argv (int argc, char **argv)
{
  bool show_help = false;
  option_parser = new Getopt_long (argc, argv, options_static);
  while (Long_option_init const *opt = (*option_parser) ())
    {
      switch (opt->shortname_char_)
        {
        case 'f':
          {
            std::string arg = option_parser->optional_argument_str0_;
            for (std::string a : string_split (arg, ','))
              add_output_format (a);
          }
          break;

        case 0:
          if (std::string (opt->longname_str0_) == "pdf"
              || std::string (opt->longname_str0_) == "png"
              || std::string (opt->longname_str0_) == "ps"
              || std::string (opt->longname_str0_) == "svg")
            add_output_format (opt->longname_str0_);
          else if (std::string (opt->longname_str0_) == "relocate")
            warning (_ ("The --relocate option is no longer relevant."));
          break;

        case 'E':
          init_scheme_variables_global.push_back (
            std::pair ("separate-page-formats", "eps"));
          init_scheme_variables_global.push_back (
            std::pair ("tall-page-formats", "eps"));
          break;

        case 'O':
          {
            std::string arg (option_parser->optional_argument_str0_);
            if (arg == "size")
              {
                init_scheme_variables_global.push_back (
                  std::pair ("music-font-encodings", "#f"));
                init_scheme_variables_global.push_back (
                  std::pair ("gs-never-embed-fonts", "#f"));
              }
            else if (arg == "TeX-GS")
              {
                init_scheme_variables_global.push_back (
                  std::pair ("music-font-encodings", "#t"));
                init_scheme_variables_global.push_back (
                  std::pair ("gs-never-embed-fonts", "#t"));
              }
            else if (arg == "TeX")
              {
                init_scheme_variables_global.push_back (
                  std::pair ("music-font-encodings", "#t"));
                init_scheme_variables_global.push_back (
                  std::pair ("gs-never-embed-fonts", "#f"));
              }
            else
              programming_error ("Ignoring unknown optimization key");
          }
          break;

        case 'd':
          {
            std::string arg (option_parser->optional_argument_str0_);
            ssize eq = arg.find ('=');

            std::string key = arg;
            std::string val = "#t";

            if (eq != NPOS)
              {
                key = arg.substr (0, eq);
                val = arg.substr (eq + 1, arg.length () - 1);
              }

            init_scheme_variables_global.push_back (std::pair (key, val));
          }
          break;

        case 'v':
          notice ();
          exit (0);
          break;
        case 'o':
          {
            std::string s = option_parser->optional_argument_str0_;
            File_name file_name (s);
            output_name_global = file_name.to_string ();
          }
          break;
        case 'j':
          jail_spec = option_parser->optional_argument_str0_;
          break;

        case 'e':
          init_scheme_code_global
            += option_parser->optional_argument_str0_ + std::string (" ");
          break;
        case 'w':
          warranty ();
          exit (0);
          break;

        case 'H':
          dump_header_fieldnames_global.push_back (
            option_parser->optional_argument_str0_);
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
          programming_error (
            to_string ("unhandled short option: %c", opt->shortname_char_));
          assert (false);
          break;
        }
    }

  if (output_formats_global.empty ())
    add_output_format ("pdf");

  if (output_formats_global.size () == 1 && output_formats_global[0] == "svg")
    {
      bool have_backend = false;
      for (const auto keyval : init_scheme_variables_global)
        {
          if (keyval.first == "backend")
            {
              have_backend = true;
              break;
            }
        }
      if (!have_backend)
        init_scheme_variables_global.push_back (std::pair ("backend", "svg"));
    }

  if (show_help)
    {
      ly_usage ();
      if (is_loglevel (LOG_DEBUG))
        dir_info (stdout);
      exit (0);
    }
}

static void
setup_guile_env ()
{
  sane_putenv ("GUILE_AUTO_COMPILE", "0", false); // disable auto-compile
  sane_putenv ("GUILE_WARN_DEPRECATED", "detailed",
               true); // set Guile to info re deprecation

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

int
main (int argc, char **argv)
{
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
  setup_guile_env (); // set up environment variables to pass into Guile API

#if !GS_API
  // Let Guile know whether the Ghostscript API is not available.
  init_scheme_variables_global.push_back (std::pair ("gs-api", "#f"));
#endif

  // Start up Guile API using main_with_guile as a callback.
  scm_boot_guile (argc, argv, main_with_guile, 0);

  /* Only reachable if Guile exits.  That is an error.  */
  return 1;
}
