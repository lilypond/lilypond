/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "dimensions.hh"
#include "file-name.hh"
#include "file-path.hh"
#include "international.hh"
#include "lily-guile.hh"
#include "ly-scm-list.hh"
#include "main.hh"
#include "memory.hh"
#include "misc.hh"
#include "program-option.hh"
#include "relocate.hh"
#include "std-vector.hh"
#include "string-convert.hh"
#include "version.hh"
#include "warn.hh"

#if GS_API
#include <ghostscript/iapi.h>
#include <ghostscript/ierrors.h>
#endif

#include <cstdint>
#include <cstdio>
#include <cstring> /* memset */
#include <ctype.h>
#include <glib.h>
#include <set>
#include <string>

using std::string;
using std::vector;

/* Declaration of log function(s) */
SCM ly_progress (SCM, SCM);

LY_DEFINE (ly_find_file, "ly:find-file", 1, 0, 0, (SCM name),
           R"(
Return the absolute file name of @var{name}, or @code{#f} if not found.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, name, 1);

  string nm = ly_scm2string (name);
  string file_name = global_path.find (nm);
  if (file_name.empty ())
    return SCM_BOOL_F;

  return ly_string2scm (file_name);
}

LY_DEFINE (ly_rename_file, "ly:rename-file", 2, 0, 0,
           (SCM oldname, SCM newname),
           R"(
Rename @var{oldname} to @var{newname}. In contrast to Guile's
@code{rename-file} function, this replaces the destination if it already
exists.  On Windows, fall back to copying the file contents if @var{newname}
cannot be deleted.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, oldname, 1);
  LY_ASSERT_TYPE (scm_is_string, newname, 1);

  string oldname_s = ly_scm2string (oldname);
  string newname_s = ly_scm2string (newname);

  if (!rename_file (oldname_s.c_str (), newname_s.c_str ()))
    {
      error (_f ("cannot rename `%s' to `%s'", oldname_s.c_str (),
                 newname_s.c_str ()));
    }

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_randomize_rand_seed, "ly:randomize-rand-seed", 0, 0, 0, (),
           R"(
Randomize C random generator.
           )")
{
  struct timeval tv = {};
  pid_t pid = getpid ();
  gettimeofday (&tv, NULL);
  srand (static_cast<unsigned> (tv.tv_sec ^ tv.tv_usec ^ pid));
  return SCM_UNSPECIFIED;
}

/*
  Ugh. Gulped file is copied twice. (maybe thrice if you count stdio
  buffering.)
*/
LY_DEFINE (ly_gulp_file, "ly:gulp-file", 1, 1, 0, (SCM name, SCM size),
           R"(
Read @var{size} characters from the file @var{name}, and return its contents in
a string.  If @var{size} is undefined, the entire file is read.  The file is
looked up using the search path.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, name, 1);
  int sz = INT_MAX;
  if (!SCM_UNBNDP (size))
    {
      LY_ASSERT_TYPE (scm_is_number, size, 2);
      sz = from_scm<int> (size);
    }

  string contents = gulp_file_to_string (ly_scm2string (name), true, sz);
  return scm_from_latin1_stringn (contents.c_str (), contents.length ());
}

LY_DEFINE (ly_gulp_file_utf8, "ly:gulp-file-utf8", 1, 1, 0,
           (SCM name, SCM size),
           R"(
Read @var{size} characters from the file @var{name}, and return its contents in
a string decoded from UTF-8.  If @var{size} is undefined, the entire file is
read.  The file is looked up using the search path.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, name, 1);
  int sz = INT_MAX;
  if (!SCM_UNBNDP (size))
    {
      LY_ASSERT_TYPE (scm_is_number, size, 2);
      sz = from_scm<int> (size);
    }

  string contents = gulp_file_to_string (ly_scm2string (name), true, sz);
  return scm_from_utf8_stringn (contents.c_str (), contents.length ());
}

LY_DEFINE (ly_dir_p, "ly:dir?", 1, 0, 0, (SCM s),
           R"(
Is @var{s} a direction?  Valid directions are @w{@code{-1}}, @code{0},
or@tie{}@code{1}, where @w{@code{-1}} represents left or down,
@code{1}@tie{}represents right or up, and @code{0} represents a neutral
direction.
           )")
{
  if (scm_is_integer (s))
    {
      int i = from_scm<int> (s);
      return (i >= -1 && i <= 1) ? SCM_BOOL_T : SCM_BOOL_F;
    }
  return SCM_BOOL_F;
}

LY_DEFINE (ly_assoc_get, "ly:assoc-get", 2, 2, 0,
           (SCM key, SCM alist, SCM default_value, SCM strict_checking),
           R"(
Return value if @var{key} in @var{alist}, else @var{default-value} (or
@code{#f} if not specified).  If @var{strict-checking} is set to @code{#t} and
@var{key} is not in @var{alist}, a programming error is output.
           )")
{
  LY_ASSERT_TYPE (ly_cheap_is_list, alist, 2);

  SCM handle = ly_assoc (key, alist);
  if (scm_is_pair (handle))
    return scm_cdr (handle);

  if (SCM_UNBNDP (default_value))
    default_value = SCM_BOOL_F;

  if (from_scm<bool> (strict_checking))
    {
      string key_string
        = ly_scm2string (scm_object_to_string (key, SCM_UNDEFINED));
      string default_value_string
        = ly_scm2string (scm_object_to_string (default_value, SCM_UNDEFINED));
      programming_error ("Cannot find key `" + key_string
                         + "' in alist, setting to `" + default_value_string
                         + "'.");
    }

  return default_value;
}

LY_DEFINE (ly_string_substitute, "ly:string-substitute", 3, 0, 0,
           (SCM a, SCM b, SCM s),
           R"(
Replace string@tie{}@var{a} by string@tie{}@var{b} in string@tie{}@var{s}.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, s, 1);
  LY_ASSERT_TYPE (scm_is_string, b, 2);
  LY_ASSERT_TYPE (scm_is_string, s, 3);

  string ss = ly_scm2string (s);
  replace_all (&ss, ly_scm2string (a), ly_scm2string (b));

  return ly_string2scm (ss);
}

LY_DEFINE (ly_string_percent_encode, "ly:string-percent-encode", 1, 0, 0,
           (SCM str),
           R"(
Encode all characters in string @var{str} with hexadecimal percent escape
sequences, with the following exceptions: characters @w{@samp{-./_}} and
characters in ranges @code{0-9}, @code{A-Z}, and @code{a-z}.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);

  string orig_str = ly_scm2string (str);
  string new_str = String_convert::percent_encode (orig_str);

  return ly_string2scm (new_str);
}

LY_DEFINE (ly_number_2_string, "ly:number->string", 1, 0, 0, (SCM s),
           R"(
Convert @var{s} to a string without generating many decimals.
           )")
{
  LY_ASSERT_TYPE (scm_is_number, s, 1);

  char str[400]; // ugh.

  if (scm_is_false (scm_exact_p (s)))
    {
      Real r (from_scm<double> (s));
      if (!std::isfinite (r))
        {
          programming_error (
            "infinity or NaN encountered while converting Real number, "
            "setting to zero");

          r = 0.0;
        }

      snprintf (str, sizeof (str), "%.4f", r);
    }
  else
    snprintf (str, sizeof (str), "%lld", from_scm<long long> (s));

  return scm_from_latin1_string (str);
}

LY_DEFINE (ly_version, "ly:version", 0, 0, 0, (),
           R"(
Return the current LilyPond version as a list, e.g., @code{(1 3 127 uu1)}.
           )")
{
  SCM major = scm_string_to_number (ly_string2scm (MAJOR_VERSION), to_scm (10));
  SCM minor = scm_string_to_number (ly_string2scm (MINOR_VERSION), to_scm (10));
  SCM patch = scm_string_to_number (ly_string2scm (PATCH_LEVEL), to_scm (10));
  if (MY_PATCH_LEVEL[0] == '\0')
    return ly_list (major, minor, patch);
  else
    return ly_list (major, minor, patch, ly_symbol2scm (MY_PATCH_LEVEL));
}

LY_DEFINE (ly_unit, "ly:unit", 0, 0, 0, (),
           R"(
Return the unit used for lengths as a string.
           )")
{
  return scm_from_latin1_string (INTERNAL_UNIT);
}

LY_DEFINE (ly_dimension_p, "ly:dimension?", 1, 0, 0, (SCM d),
           R"(
Is @var{d} a dimension?  Used to distinguish length variables from normal
numbers.
           )")
{
  return scm_number_p (d);
}

// TODO: When we drop Guile 1 support, remove this function
// and simply use either escape sequences (\u, \U) and/or
// integer->char.
LY_DEFINE (ly_wide_char_2_utf_8, "ly:wide-char->utf-8", 1, 0, 0, (SCM wc),
           R"(
Encode the Unicode codepoint @var{wc}, an integer, as UTF-8.
           )")
{
  char buf[5];

  // TODO: The input to ly:wide-char->utf-8 is not a char.  Rename?
  LY_ASSERT_TYPE (ly_is_unicode_integer, wc, 1);
  const auto wide_char = from_scm<std::uint32_t> (wc);
  char *p = buf;

  if (wide_char < 0x0080)
    *p++ = static_cast<char> (wide_char);
  else if (wide_char < 0x0800)
    {
      *p++ = static_cast<char> (((wide_char >> 6)) | 0xC0);
      *p++ = static_cast<char> (((wide_char) &0x3F) | 0x80);
    }
  else if (wide_char < 0x10000)
    {
      *p++ = static_cast<char> (((wide_char >> 12)) | 0xE0);
      *p++ = static_cast<char> (((wide_char >> 6) & 0x3F) | 0x80);
      *p++ = static_cast<char> (((wide_char) &0x3F) | 0x80);
    }
  else
    {
      *p++ = static_cast<char> (((wide_char >> 18)) | 0xF0);
      *p++ = static_cast<char> (((wide_char >> 12) & 0x3F) | 0x80);
      *p++ = static_cast<char> (((wide_char >> 6) & 0x3F) | 0x80);
      *p++ = static_cast<char> (((wide_char) &0x3F) | 0x80);
    }
  *p = 0;

  return scm_from_utf8_string (buf);
}

LY_DEFINE (ly_effective_prefix, "ly:effective-prefix", 0, 0, 0, (),
           R"(
Return effective prefix.  For example, if LilyPond Scheme files are stored in
directory @file{/foo/bar/scm} and PS files in @file{/foo/bar/ps}, the effective
prefix is @file{/foo/bar}.
           )")
{
  return ly_string2scm (lilypond_datadir);
}

LY_DEFINE (ly_chain_assoc_get, "ly:chain-assoc-get", 2, 2, 0,
           (SCM key, SCM achain, SCM default_value, SCM strict_checking),
           R"(
Return value for @var{key} from a list of alists @var{achain}.  If no entry is
found, return @var{default-value} or @code{#f} if @var{default-value} is not
specified.  With @var{strict-checking} set to @code{#t}, a programming error is
output in such cases.
           )")
{
  if (scm_is_pair (achain))
    {
      SCM handle = scm_is_symbol (key) ? scm_assq (key, scm_car (achain))
                                       : ly_assoc (key, scm_car (achain));
      if (scm_is_pair (handle))
        return scm_cdr (handle);
      else
        return ly_chain_assoc_get (key, scm_cdr (achain), default_value);
    }

  if (from_scm<bool> (strict_checking))
    {
      string key_string
        = ly_scm2string (scm_object_to_string (key, SCM_UNDEFINED));
      string default_value_string
        = ly_scm2string (scm_object_to_string (default_value, SCM_UNDEFINED));
      programming_error ("Cannot find key `" + key_string
                         + "' in achain, setting to `" + default_value_string
                         + "'.");
    }

  return SCM_UNBNDP (default_value) ? SCM_BOOL_F : default_value;
}

LY_DEFINE (ly_stderr_redirect, "ly:stderr-redirect", 1, 1, 0,
           (SCM fd_or_file_name, SCM mode),
           R"(
Redirect standard error output (stderr) to file descriptor @var{fd} if the
first parameter is an integer, or to file @var{file-name}, opened with
@var{mode}.
           )")
{
  fflush (stderr);

  if (scm_is_integer (fd_or_file_name))
    {
      // If passed a file descriptor, just replace the error stream (fd 2) by
      // calling dup2.
      int fd = from_scm<int> (fd_or_file_name);
      if (dup2 (fd, 2) == -1)
        error (_ ("failed redirecting stderr"));
      return SCM_UNSPECIFIED;
    }

  LY_ASSERT_TYPE (scm_is_string, fd_or_file_name, 1);

  string m = "w";
  string f = ly_scm2string (fd_or_file_name);
  FILE *stderrfile;
  if (scm_is_string (mode))
    m = ly_scm2string (mode);
  stderrfile = freopen (f.c_str (), m.c_str (), stderr);
  if (!stderrfile)
    error (_f ("failed redirecting stderr to `%s'", f.c_str ()));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_hash_table_keys, "ly:hash-table-keys", 1, 0, 0, (SCM tab),
           R"(
Return a list of keys in @var{tab}.
           )")
{
  auto accumulate_symbol = [] (void * /* closure */, SCM key, SCM /* val */,
                               SCM result) { return scm_cons (key, result); };

  return ly_scm_hash_fold (accumulate_symbol, nullptr, SCM_EOL, tab);
}

LY_DEFINE (ly_camel_case_2_lisp_identifier, "ly:camel-case->lisp-identifier", 1,
           0, 0, (SCM name_sym),
           R"(
Convert @code{FooBar_Bla} to @code{foo-bar-bla} style symbol.
           )")
{
  LY_ASSERT_TYPE (ly_is_symbol, name_sym, 1);

  /*
    TODO: should use strings instead?
  */

  const string in = ly_symbol2string (name_sym);
  string result = camel_case_to_lisp_identifier (in);

  return ly_symbol2scm (result);
}

string
format_single_argument (SCM arg, int precision, bool escape = false)
{
  if (scm_is_integer (arg) && scm_is_true (scm_exact_p (arg)))
    return std::to_string (from_scm<int> (arg));
  else if (scm_is_number (arg))
    {
      Real val = from_scm<double> (arg);

      if (!std::isfinite (val))
        {
          warning (_ ("Found infinity or nan in output.  Substituting 0.0"));
          return ("0.0");
          if (strict_infinity_checking)
            abort ();
        }
      else
        return (String_convert::form_string ("%.*lf", precision, val));
    }
  else if (scm_is_string (arg))
    {
      string s = ly_scm2string (arg);
      if (escape)
        {
          // Escape backslashes and double quotes, wrap it in double quotes
          replace_all (&s, "\\", "\\\\");
          replace_all (&s, "\"", "\\\"");
          // don't replace percents, since the png backend uses %d as escape sequence
          // replace_all (&s, "%", "\\%");
          replace_all (&s, "$", "\\$");
          s = "\"" + s + "\"";
        }
      return s;
    }
  else if (scm_is_symbol (arg))
    return (ly_symbol2string (arg));
  else
    {
      ly_progress (
        scm_from_latin1_string ("\nUnsupported SCM value for format: ~a"),
        ly_list (arg));
    }

  return "";
}

LY_DEFINE (ly_format, "ly:format", 1, 0, 1, (SCM str, SCM rest),
           R"(
LilyPond specific format function, supporting @code{~a} and @code{~[0-9]f}.
Basic support for @code{~s} is also provided.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);

  string format = ly_scm2string (str);
  vector<string> results;

  vsize i = 0;
  while (i < format.size ())
    {
      vsize tilde = format.find ('~', i);

      results.push_back (format.substr (i, (tilde - i)));

      if (tilde == NPOS)
        break;

      tilde++;

      char spec = format.at (tilde++);
      if (spec == '~')
        results.push_back ("~");
      else
        {
          if (!scm_is_pair (rest))
            {
              programming_error (string (__FUNCTION__)
                                 + ": not enough arguments for format.");
              return ly_string2scm ("");
            }

          SCM arg = scm_car (rest);
          rest = scm_cdr (rest);

          int precision = 8;

          if (spec == '$')
            precision = 2;
          else if (isdigit (spec))
            {
              precision = spec - '0';
              spec = format.at (tilde++);
            }

          if (spec == 'a' || spec == 'A' || spec == 'f' || spec == '$')
            results.push_back (format_single_argument (arg, precision));
          else if (spec == 's' || spec == 'S')
            results.push_back (format_single_argument (arg, precision, true));
        }

      i = tilde;
    }

  if (scm_is_pair (rest))
    programming_error (string (__FUNCTION__) + ": too many arguments");

  // one wonders how much this extra walk actually saves
  vsize len = 0;
  for (const auto &r : results)
    len += r.size ();

  string result;
  result.reserve (len);
  for (const auto &r : results)
    result.append (r);

  SCM ret = scm_from_utf8_stringn (result.data (), result.size ());
  return ret;
}

int
ly_run_command (char *argv[], char **standard_output, char **standard_error)
{
  GError *error = 0;
  int exit_status = 0;
  int flags = G_SPAWN_SEARCH_PATH;
  if (!standard_output)
    flags |= G_SPAWN_STDOUT_TO_DEV_NULL;
  if (!standard_error)
    flags |= G_SPAWN_STDERR_TO_DEV_NULL;
  if (!g_spawn_sync (0, argv, 0, GSpawnFlags (flags), 0, 0, standard_output,
                     standard_error, &exit_status, &error))
    {
      if (!exit_status)
        exit_status = -1;
      warning (_f ("g_spawn_sync failed (%d): %s: %s", exit_status, argv[0],
                   error->message));
      g_error_free (error);
    }

  return exit_status;
}

LY_DEFINE (ly_spawn, "ly:spawn", 1, 0, 1, (SCM command, SCM rest),
           R"(
Simple Scheme interface to the GLib function @code{g_spawn_sync}.  If an error
occurs, format it with @code{format} and @var{rest}.
           )")

{
  LY_ASSERT_TYPE (scm_is_string, command, 1);

  const auto argc = 1 + (scm_is_pair (rest) ? scm_ilength (rest) : 0);

  // copy the command line (as individual arguments)
  std::vector<unique_stdlib_ptr<char>> own_argv;
  own_argv.reserve (argc);
  own_argv.emplace_back (ly_scm2str0 (command));
  for (SCM s : as_ly_scm_list (rest))
    own_argv.emplace_back (ly_scm2str0 (s));

  // create a C-style argv aliasing the arguments above
  std::vector<char *> argv;
  argv.reserve (own_argv.size () + 1);
  for (const auto &p : own_argv)
    argv.emplace_back (p.get ());
  argv.emplace_back (nullptr);

  char *standard_output = 0;
  char *standard_error = 0;
  // Always get the pointer to the stdout/stderr messages
  const auto exit_status
    = ly_run_command (argv.data (), &standard_output, &standard_error);

  if (standard_output && standard_error)
    {
      // Print out stdout and stderr only in debug mode
      debug_output (string ("\n") + standard_output + standard_error, true);
    }

  g_free (standard_error);
  g_free (standard_output);

  return to_scm (exit_status);
}

#if GS_API
static void *gs_inst = NULL;
static string gs_args;

LY_DEFINE (ly_shutdown_gs, "ly:shutdown-gs", 0, 0, 0, (),
           R"(
Shutdown GhostScript instance and flush pending writes.
           )")
{
  if (gs_inst == NULL)
    {
      assert (gs_args.length () == 0);
      return SCM_UNDEFINED;
    }

  debug_output (_ ("Exiting current GhostScript instance...\n"));

  int exit_code;
  gsapi_run_string (gs_inst, "quit", 0, &exit_code);
  gsapi_exit (gs_inst);
  gsapi_delete_instance (gs_inst);

  gs_inst = NULL;
  gs_args = "";

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_gs_api, "ly:gs-api", 2, 0, 0, (SCM args, SCM run_string),
           R"(
Use GhostScript started with @var{args}, and run @var{run_string}
           )")
{
  LY_ASSERT_TYPE (scm_is_pair, args, 1);
  LY_ASSERT_TYPE (scm_is_string, run_string, 2);

  {
    // gsapi_init_with_args wants modifiable strings, so create local variables
    // with copies of the content.
    std::vector<unique_stdlib_ptr<char>> own_argv;

    // Ensure that the string of arguments is never empty.
    string new_args (" ");
    for (SCM s : as_ly_scm_list (args))
      {
        auto a (ly_scm2str0 (s));
        new_args += a.get ();
        new_args += ' ';
        own_argv.emplace_back (std::move (a));
      }

    if (gs_args.length () > 0)
      {
        assert (gs_inst != NULL);
        if (gs_args != new_args)
          {
            debug_output (_ ("Mismatch of GhostScript arguments!\n"));
            ly_shutdown_gs ();
          }
      }

    if (gs_inst == NULL)
      {
        debug_output (_f ("Starting GhostScript instance with arguments: %s\n",
                          new_args.c_str ()));
        // Save current string of arguments to later compare if we need a new
        // instance with different parameters.
        gs_args = new_args;

        int code = gsapi_new_instance (&gs_inst, NULL);
        if (code == 0)
          code = gsapi_set_arg_encoding (gs_inst, GS_ARG_ENCODING_UTF8);
        if (code == 0)
          {
            // create a C-style argv aliasing the arguments above
            std::vector<char *> argv;
            argv.reserve (own_argv.size ());
            for (const auto &p : own_argv)
              argv.emplace_back (p.get ());

            const auto argc = static_cast<int> (argv.size ());
            code = gsapi_init_with_args (gs_inst, argc, argv.data ());
          }

        // Handle errors from above calls.
        if (code < 0)
          {
            warning (_ ("Could not start GhostScript instance!"));
            scm_throw (ly_symbol2scm ("ly-file-failed"), ly_list (run_string));
            return SCM_UNSPECIFIED;
          }
      }
  }

  // Construct the command.
  string command = ly_scm2string (run_string);

  debug_output (_f ("Running GhostScript command: %s\n", command.c_str ()));

  int exit_code;
  int code = gsapi_run_string (gs_inst, command.c_str (), 0, &exit_code);
  // gs_error_invalidexit could be avoided by having a 'quit' at the end of
  // the command. However this leads to execstackoverflow when running many
  // conversions, for example when compiling the Notation Reference.
  if (code != 0 && code != gs_error_Quit && code != gs_error_invalidexit)
    {
      warning (_ ("Error when running GhostScript command!"));
      scm_throw (ly_symbol2scm ("ly-file-failed"), ly_list (run_string));
    }

  return SCM_UNSPECIFIED;
}
#endif
