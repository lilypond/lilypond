/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "relocate.hh"

/* TODO: autoconf support */

#include "file-name.hh"
#include "file-path.hh"
#include "international.hh"
#include "lily-guile.hh"
#include "main.hh"
#include "version.hh"
#include "warn.hh"

#if HAVE_GETTEXT
#include <libintl.h>
#endif

#ifdef __MINGW32__
#include <winbase.h>
#endif

#include <dirent.h>
#include <sys/types.h>

#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

using std::string;
using std::vector;

#define FRAMEWORKDIR ".."

int
sane_putenv (char const *key, const string &value, bool overwrite, bool indent)
{
  const char *space = indent ? "  " : "";

  if (overwrite || !getenv (key))
    {
      string combine = string (key) + "=" + value;
      char *s = strdup (combine.c_str ());

      debug_output (_f ("%sSetting %s to '%s'\n", space, key, value));

      int retval = putenv (s);
      /*
        unfortunately, we can't portably free S here,
        due to various bugs in glibc prior to 2.1.1
      */
      return retval;
    }
  else
    debug_output (_f ("%s%s not overwritten\n", space, key));

  return -1;
}

static int
set_env_file (char const *key, const string &value, bool overwrite = false)
{
  if (is_file (value))
    return sane_putenv (key, value, overwrite);
  else if (is_loglevel (LOG_DEBUG))
    warning (_f ("No such file '%s' for %s", value, key));
  return -1;
}

static int
set_env_dir (char const *key, const string &value)
{
  if (is_dir (value))
    return sane_putenv (key, value, false);
  else if (is_loglevel (LOG_DEBUG))
    warning (_f ("No such directory '%s' for %s", value, key));
  return -1;
}

static int
prepend_env_path (char const *key, string value)
{
  if (is_dir (value))
    {
      debug_output (_f ("  Prepending '%s' to %s\n", value, key));

      if (char const *cur = getenv (key))
        {
          value += PATHSEP;
          value += cur;
        }

      return sane_putenv (key, value.c_str (), true, true);
    }
  else if (is_loglevel (LOG_DEBUG))
    warning (_f ("No such directory '%s' for %s", value, key));
  return -1;
}

static string
set_up_directory (char const *env_name, char const *id, string compile_default,
                  string runtime_default, string alt_runtime_default = "")
{
  string dir = "";

  // check environment variable
  if (char const *env_value = getenv (env_name))
    {
      dir = File_name (env_value).canonicalized ().to_string ();
      debug_output (_f ("  Found %s environment variable,\n"
                        "    setting %s to '%s'\n",
                        env_name, id, dir));
      return dir;
    }

  // otherwise check run-time value(s)
  if (is_dir (runtime_default))
    dir = runtime_default;
  else if (is_dir (alt_runtime_default))
    dir = alt_runtime_default;

  if (!dir.empty ())
    {
      dir = File_name (dir).canonicalized ().to_string ();
      debug_output (_f ("  Using run-time value for %s,\n"
                        "    setting it to '%s'\n",
                        id, dir));
      return dir;
    }

  // otherwise fall back to compile-time value
  dir = File_name (compile_default).canonicalized ().to_string ();
  debug_output (_f ("  Using compile-time value for %s,\n"
                    "    setting it to '%s'\n",
                    id, dir));
  return dir;
}

void
setup_paths (char const *argv0_ptr)
{
  File_name argv0_filename (argv0_ptr);

  debug_output (_ ("\n"
                   "Relocation\n"));

  // compute absolute path to LilyPond binary
  string argv0_abs;
  if (argv0_filename.is_absolute ())
    {
      argv0_abs = argv0_filename.to_string ();
      debug_output (_f ("  LilyPond binary has absolute file name:\n"
                        "    %s\n",
                        argv0_ptr));
    }
  else if (argv0_filename.dir_.length ())
    {
      argv0_abs = get_working_directory () + "/" + argv0_filename.to_string ();
      debug_output (_f ("  LilyPond binary has relative file name:\n"
                        "    %s\n",
                        argv0_ptr));
    }
  else
    {
      // find absolute ARGV0 name, using PATH
      File_path path;
      char *p = getenv ("PATH");
      if (p)
        path.parse_path (p);

#ifndef __MINGW32__
      argv0_abs = path.find (argv0_filename.to_string ());
#else  // __MINGW32__
      path.prepend (get_working_directory ());
      char const *ext[] = {"exe", "", 0};
      argv0_abs = path.find (argv0_filename.to_string (), ext);
#endif // __MINGW32__

      debug_output (
        _f ("  Absolute file name of LilyPond binary computed from PATH:\n"
            "    PATH=%s\n"
            "    argv0=%s\n",
            path.to_string (), argv0_ptr));

      if (argv0_abs.empty ())
        programming_error ("cannot find absolute argv0");
    }

#ifndef __MINGW32__
  // After finding the absolute path, look through symlinks if there is a
  // share/lilypond directory next to actual executable. The fallback is
  // needed for executing out/bin/lilypond during the build process.
  char resolved_path[PATH_MAX];
  char *res = realpath (argv0_abs.c_str (), resolved_path);
  if (res != NULL)
    {
      std::string share_lilypond (dir_name (resolved_path));
      share_lilypond += "/../share/lilypond/";
      struct stat st;
      if (stat (share_lilypond.c_str (), &st) == 0 && S_ISDIR (st.st_mode))
        argv0_abs = resolved_path;
    }
#endif

  string bindir
    = File_name (dir_name (argv0_abs)).canonicalized ().to_string ();
  string prefix = File_name (bindir + "/..").canonicalized ().to_string ();

  // set INSTALLER_PREFIX environment variable
  sane_putenv ("INSTALLER_PREFIX", prefix.c_str (), true, true);

  // get values for LilyPond's data directories
  if (getenv ("LILYPONDPREFIX"))
    error (_ ("LILYPONDPREFIX is obsolete, use LILYPOND_DATADIR"));

  lilypond_datadir = set_up_directory (
    "LILYPOND_DATADIR", "datadir", PACKAGE_DATADIR "/" TOPLEVEL_VERSION,
    prefix + "/share/lilypond/" TOPLEVEL_VERSION,
    prefix + "/share/lilypond/current");
  lilypond_libdir = set_up_directory (
    "LILYPOND_LIBDIR", "libdir", PACKAGE_LIBDIR "/" TOPLEVEL_VERSION,
    prefix + "/lib/lilypond/" TOPLEVEL_VERSION,
    prefix + "/lib/lilypond/current");
  string localedir = set_up_directory ("LILYPOND_LOCALEDIR", "localedir",
                                       LOCALEDIR, prefix + "/share/locale");
  string relocdir = set_up_directory ("LILYPOND_RELOCDIR", "relocdir",
                                      "", // no compile-time default
                                      prefix + "/etc/relocate");

#if HAVE_GETTEXT
  if (is_dir (localedir))
    bindtextdomain ("lilypond", localedir.c_str ());
#endif

  if (is_dir (relocdir))
    read_relocation_dir (relocdir);

  prepend_env_path ("PATH", bindir);
  global_path.append ("");

  // add some datadir subdirectories to LilyPond's global path;
  // in this list, the last item comes first in the path
  global_path.prepend (lilypond_datadir + "/fonts/svg/");
  global_path.prepend (lilypond_datadir + "/fonts/otf/");
  global_path.prepend (lilypond_datadir + "/scm");
  global_path.prepend (lilypond_datadir + "/ps");
  global_path.prepend (lilypond_datadir + "/ly");
}

static string
expand_environment_variables (const string &orig)
{
  char const *start_ptr = orig.c_str ();
  char const *ptr = orig.c_str ();
  size_t len = orig.length ();

  string out;
  while (ptr < start_ptr + len)
    {
      char const *dollar = strchr (ptr, '$');

      if (dollar != NULL)
        {
          char const *start_var = dollar + 1;
          char const *end_var = start_var;
          char const *start_next = end_var;

          out += string (ptr, dollar - ptr);
          ptr = dollar;

          if (*start_var == '{')
            {
              start_var++;

              end_var = strchr (start_var, '}');

              if (end_var == NULL)
                {
                  end_var = start_var + len;
                  start_next = end_var;
                }
              else
                {
                  start_next = end_var + 1;
                }
            }
          else
            {
              /*
                Hmm. what to do for $1 , $~ etc.?
              */
              do
                {
                  end_var++;
                }
              while (isalnum (*end_var) || *end_var == '_');
              start_next = end_var;
            }

          if (start_var < end_var)
            {
              string var_name (start_var, end_var - start_var);
              char const *value = getenv (var_name.c_str ());
              if (value != NULL)
                out += string (value);

              ptr = start_next;
            }
        }
      else
        break;
    }

  out += ptr;

  return out;
}

// Ugh - very inefficient, but safer than fgets.
static string
read_line (FILE *f)
{
  string out;

  int c = 0;
  while ((c = fgetc (f)) != EOF && c != '\n')
    out += char (c);

  return out;
}

void
read_relocation_file (const string &filename)
{
  debug_output (_f ("  Relocation file '%s'\n", filename));
  char const *cname = filename.c_str ();
  FILE *f = fopen (cname, "r");
  if (!f)
    error (_f ("cannot open file '%s', ignored", cname));

  while (!feof (f))
    {
      string line = read_line (f);
      size_t idx = line.find (' ');
      if (idx == NPOS)
        continue;

      string command = line.substr (0, idx);
      line = line.substr (idx + 1);

      if (idx == NPOS)
        continue;
      idx = line.find ('=');

      string variable = line.substr (0, idx);
      string value = line.substr (idx + 1);

      value = expand_environment_variables (value);

      if (command == "set")
        sane_putenv (variable.c_str (), value, true, true);
      else if (command == "set?")
        sane_putenv (variable.c_str (), value, false, true);
      else if (command == "setdir")
        set_env_dir (variable.c_str (), value);
      else if (command == "setfile")
        set_env_file (variable.c_str (), value);
      else if (command == "prependdir")
        prepend_env_path (variable.c_str (), value);
      else
        error (_f ("Unknown relocation command '%s'", command));
    }

  fclose (f);
}

void
read_relocation_dir (const string &dirname)
{
  if (DIR *dir = opendir (dirname.c_str ()))
    {
      if (is_loglevel (LOG_DEBUG))
        debug_output (_f ("\n"
                          "  Using relocation config directory '%s'\n",
                          dirname));
      while (struct dirent *ent = readdir (dir))
        {
          File_name name (ent->d_name);
          if (name.ext_ == "reloc")
            read_relocation_file (dirname + "/" + name.to_string ());
        }
    }
  else if (is_loglevel (LOG_DEBUG))
    warning (_f ("No relocation config directory '%s'", dirname));
}
