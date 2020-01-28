/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Jan Nieuwenhuizen <janneke@gnu.org>
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

#include <cstdio>
#include <cstring> /* memset */
#include <ctype.h>
#include <glib.h>

#include "dimensions.hh"
#include "file-name.hh"
#include "file-path.hh"
#include "international.hh"
#include "libc-extension.hh"
#include "lily-guile.hh"
#include "main.hh"
#include "misc.hh"
#include "program-option.hh"
#include "relocate.hh"
#include "string-convert.hh"
#include "version.hh"
#include "warn.hh"

using std::string;
using std::vector;

/* Declaration of log function(s) */
SCM ly_progress (SCM, SCM);

LY_DEFINE (ly_start_environment, "ly:start-environment", 0, 0, 0, (),
           "Return the environment (a list of strings) that was in"
           " effect at program start.")
{
  SCM l = SCM_EOL;
  SCM *tail = &l;

  for (vsize i = 0; i < start_environment_global.size (); i++)
    {
      *tail = scm_cons (ly_string2scm (start_environment_global[i]), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }

  return l;
}

LY_DEFINE (ly_find_file, "ly:find-file", 1, 0, 0, (SCM name),
           "Return the absolute file name of @var{name},"
           " or @code{#f} if not found.")
{
  LY_ASSERT_TYPE (scm_is_string, name, 1);

  string nm = ly_scm2string (name);
  string file_name = global_path.find (nm);
  if (file_name.empty ())
    return SCM_BOOL_F;

  return ly_string2scm (file_name);
}

/*
  Ugh. Gulped file is copied twice. (maybe thrice if you count stdio
  buffering.)
*/
LY_DEFINE (ly_gulp_file, "ly:gulp-file", 1, 1, 0, (SCM name, SCM size),
           "Read @var{size} characters from the file @var{name},"
           " and return its contents in a string."
           "  If @var{size} is undefined, the entire file is read."
           "  The file is looked up using the search path.")
{
  LY_ASSERT_TYPE (scm_is_string, name, 1);
  int sz = INT_MAX;
  if (!SCM_UNBNDP (size))
    {
      LY_ASSERT_TYPE (scm_is_number, size, 2);
      sz = scm_to_int (size);
    }

  string contents = gulp_file_to_string (ly_scm2string (name), true, sz);
  return scm_from_latin1_stringn (contents.c_str (), contents.length ());
}

LY_DEFINE (ly_dir_p, "ly:dir?", 1, 0, 0, (SCM s),
           "Is @var{s} a direction?  Valid directions are @w{@code{-1}},"
           " @code{0}, or@tie{}@code{1}, where @w{@code{-1}} represents"
           " left or down, @code{1}@tie{}represents right or up, and @code{0}"
           " represents a neutral direction.")
{
  if (scm_is_integer (s))
    {
      int i = scm_to_int (s);
      return (i >= -1 && i <= 1) ? SCM_BOOL_T : SCM_BOOL_F;
    }
  return SCM_BOOL_F;
}

LY_DEFINE (
    ly_assoc_get, "ly:assoc-get", 2, 2, 0,
    (SCM key, SCM alist, SCM default_value, SCM strict_checking),
    "Return value if @var{key} in @var{alist}, else @var{default-value}"
    " (or @code{#f} if not specified).  If @var{strict-checking} is set"
    " to @code{#t} and @var{key} is not in @var{alist}, a programming_error"
    " is output.")
{
  LY_ASSERT_TYPE (ly_cheap_is_list, alist, 2);

  SCM handle = scm_assoc (key, alist);
  if (scm_is_pair (handle))
    return scm_cdr (handle);

  if (SCM_UNBNDP (default_value))
    default_value = SCM_BOOL_F;

  if (to_boolean (strict_checking))
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
           "Replace string@tie{}@var{a} by string@tie{}@var{b} in"
           " string@tie{}@var{s}.")
{
  LY_ASSERT_TYPE (scm_is_string, s, 1);
  LY_ASSERT_TYPE (scm_is_string, b, 2);
  LY_ASSERT_TYPE (scm_is_string, s, 3);

  string ss = ly_scm2string (s);
  replace_all (&ss, ly_scm2string (a), ly_scm2string (b));

  return ly_string2scm (ss);
}

bool
is_not_escape_character (Byte c)
{
  switch (c)
    {
    case '-':
    case '.':
    case '/':
    case '0' ... '9':
    case ':':
    case 'A' ... 'Z':
    case '_':
    case 'a' ... 'z':
      return true;
    }

  return false;
}

LY_DEFINE (ly_string_percent_encode, "ly:string-percent-encode", 1, 0, 0,
           (SCM str),
           "Encode all characters in string @var{str} with hexadecimal"
           " percent escape sequences, with the following exceptions:"
           " characters @w{@code{-},} @code{.}, @code{/}, and @code{_}; and"
           " characters in ranges @code{0-9}, @code{A-Z}, and @code{a-z}.")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);

  string orig_str = ly_scm2string (str);
  string new_str = "";

  vsize i = 0;
  vsize n = orig_str.size ();

  while (i < n)
    {
      Byte cur = orig_str[i];

      if (is_not_escape_character (cur))
        new_str += cur;
      else
        {
          new_str += '%';
          new_str += String_convert::bin2hex (cur);
        }

      i++;
    }

  return ly_string2scm (new_str);
}

LY_DEFINE (ly_number_2_string, "ly:number->string", 1, 0, 0, (SCM s),
           "Convert @var{s} to a string without generating many decimals.")
{
  LY_ASSERT_TYPE (scm_is_number, s, 1);

  char str[400]; // ugh.

  if (scm_is_false (scm_exact_p (s)))
    {
      Real r (scm_to_double (s));
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
    snprintf (str, sizeof (str), "%d", int (scm_to_int (s)));

  return scm_from_ascii_string (str);
}

LY_DEFINE (ly_version, "ly:version", 0, 0, 0, (),
           "Return the current lilypond version as a list, e.g.,"
           " @code{(1 3 127 uu1)}.")
{
  char const *vs = "\'(" MAJOR_VERSION " " MINOR_VERSION " " PATCH_LEVEL
                   " " MY_PATCH_LEVEL ")";

  return scm_c_eval_string ((char *)vs);
}

LY_DEFINE (ly_unit, "ly:unit", 0, 0, 0, (),
           "Return the unit used for lengths as a string.")
{
  return scm_from_ascii_string (INTERNAL_UNIT);
}

LY_DEFINE (ly_dimension_p, "ly:dimension?", 1, 0, 0, (SCM d),
           "Return @var{d} as a number.  Used to distinguish length"
           " variables from normal numbers.")
{
  return scm_number_p (d);
}

/*
  Debugging mem leaks:
*/
LY_DEFINE (ly_protects, "ly:protects", 0, 0, 0, (),
           "Return hash of protected objects.")
{
  // scm_protects is available only in Guile versions before 2.1.
#if SCM_MAJOR_VERSION < 2 || SCM_MAJOR_VERSION == 2 && SCM_MINOR_VERSION < 1
  return scm_protects;
#else
  programming_error ("ly:protects is not supported in Guile 2.1");
  return SCM_EOL;
#endif
}

LY_DEFINE (ly_gettext, "ly:gettext", 1, 0, 0, (SCM original),
           "A Scheme wrapper function for @code{gettext}.")
{
  LY_ASSERT_TYPE (scm_is_string, original, 1);
  return ly_string2scm (_ (ly_scm2string (original).c_str ()));
}

LY_DEFINE (ly_output_formats, "ly:output-formats", 0, 0, 0, (),
           "Formats passed to @option{--format} as a list of strings,"
           " used for the output.")
{
  vector<string> output_formats = string_split (output_format_global, ',');

  SCM lst = SCM_EOL;
  vsize output_formats_count = output_formats.size ();
  for (vsize i = 0; i < output_formats_count; i++)
    lst = scm_cons (ly_string2scm (output_formats[i]), lst);

  return lst;
}

LY_DEFINE (ly_wide_char_2_utf_8, "ly:wide-char->utf-8", 1, 0, 0, (SCM wc),
           "Encode the Unicode codepoint @var{wc}, an integer, as UTF-8.")
{
  char buf[5];

  LY_ASSERT_TYPE (scm_is_integer, wc, 1);
  unsigned wide_char = (unsigned)scm_to_int (wc);
  char *p = buf;

  if (wide_char < 0x0080)
    *p++ = (char)wide_char;
  else if (wide_char < 0x0800)
    {
      *p++ = (char)(((wide_char >> 6)) | 0xC0);
      *p++ = (char)(((wide_char)&0x3F) | 0x80);
    }
  else if (wide_char < 0x10000)
    {
      *p++ = (char)(((wide_char >> 12)) | 0xE0);
      *p++ = (char)(((wide_char >> 6) & 0x3F) | 0x80);
      *p++ = (char)(((wide_char)&0x3F) | 0x80);
    }
  else
    {
      *p++ = (char)(((wide_char >> 18)) | 0xF0);
      *p++ = (char)(((wide_char >> 12) & 0x3F) | 0x80);
      *p++ = (char)(((wide_char >> 6) & 0x3F) | 0x80);
      *p++ = (char)(((wide_char)&0x3F) | 0x80);
    }
  *p = 0;

  return scm_from_utf8_string (buf);
}

LY_DEFINE (ly_effective_prefix, "ly:effective-prefix", 0, 0, 0, (),
           "Return effective prefix.")
{
  return ly_string2scm (lilypond_datadir);
}

LY_DEFINE (ly_chain_assoc_get, "ly:chain-assoc-get", 2, 2, 0,
           (SCM key, SCM achain, SCM default_value, SCM strict_checking),
           "Return value for @var{key} from a list of alists @var{achain}."
           "  If no entry is found, return @var{default-value} or @code{#f} if"
           " @var{default-value} is not specified.  With @var{strict-checking}"
           " set to @code{#t}, a programming_error is output in such cases.")
{
  if (scm_is_pair (achain))
    {
      SCM handle = scm_assoc (key, scm_car (achain));
      if (scm_is_pair (handle))
        return scm_cdr (handle);
      else
        return ly_chain_assoc_get (key, scm_cdr (achain), default_value);
    }

  if (to_boolean (strict_checking))
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
           (SCM file_name, SCM mode),
           "Redirect stderr to @var{file-name}, opened with @var{mode}.")
{
  LY_ASSERT_TYPE (scm_is_string, file_name, 1);

  string m = "w";
  string f = ly_scm2string (file_name);
  FILE *stderrfile;
  if (scm_is_string (mode))
    m = ly_scm2string (mode);
  /* dup2 and (fileno (current-error-port)) do not work with mingw'c
     gcc -mwindows.  */
  fflush (stderr);
  stderrfile = freopen (f.c_str (), m.c_str (), stderr);
  if (!stderrfile)
    error (_f ("failed redirecting stderr to `%s'", f.c_str ()));
  return SCM_UNSPECIFIED;
}

static SCM
accumulate_symbol (void * /* closure */, SCM key, SCM /* val */, SCM result)
{
  return scm_cons (key, result);
}

LY_DEFINE (ly_hash_table_keys, "ly:hash-table-keys", 1, 0, 0, (SCM tab),
           "Return a list of keys in @var{tab}.")
{
  return scm_internal_hash_fold ((scm_t_hash_fold_fn)&accumulate_symbol, NULL,
                                 SCM_EOL, tab);
}

LY_DEFINE (ly_camel_case_2_lisp_identifier, "ly:camel-case->lisp-identifier", 1,
           0, 0, (SCM name_sym),
           "Convert @code{FooBar_Bla} to @code{foo-bar-bla} style symbol.")
{
  LY_ASSERT_TYPE (ly_is_symbol, name_sym, 1);

  /*
    TODO: should use strings instead?
  */

  const string in = ly_symbol2string (name_sym);
  string result = camel_case_to_lisp_identifier (in);

  return ly_symbol2scm (result.c_str ());
}

LY_DEFINE (ly_expand_environment, "ly:expand-environment", 1, 0, 0, (SCM str),
           "Expand @code{$VAR} and @code{$@{VAR@}} in @var{str}.")
{
  LY_ASSERT_TYPE (scm_is_string, str, 1);

  return ly_string2scm (expand_environment_variables (ly_scm2string (str)));
}

LY_DEFINE (ly_truncate_list_x, "ly:truncate-list!", 2, 0, 0, (SCM lst, SCM i),
           "Take at most the first @var{i} of list @var{lst}.")
{
  LY_ASSERT_TYPE (scm_is_integer, i, 1);

  int k = scm_to_int (i);
  if (k == 0)
    lst = SCM_EOL;
  else
    {
      SCM s = lst;
      k--;
      for (; scm_is_pair (s) && k--; s = scm_cdr (s))
        ;

      if (scm_is_pair (s))
        scm_set_cdr_x (s, SCM_EOL);
    }
  return lst;
}

string
format_single_argument (SCM arg, int precision, bool escape = false)
{
  if (scm_is_integer (arg) && scm_is_true (scm_exact_p (arg)))
    return (String_convert::int_string (scm_to_int (arg)));
  else if (scm_is_number (arg))
    {
      Real val = scm_to_double (arg);

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
          // don't replace percents, since the png backend uses %d as escape
          // sequence replace_all (&s, "%", "\\%");
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
          scm_from_ascii_string ("\nUnsupported SCM value for format: ~a"),
          scm_list_1 (arg));
    }

  return "";
}

LY_DEFINE (ly_format, "ly:format", 1, 0, 1, (SCM str, SCM rest),
           "LilyPond specific format, supporting @code{~a} and @code{~[0-9]f}."
           "  Basic support for @code{~s} is also provided.")
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
          else if (spec == 'l')
            {
              SCM s = arg;
              for (; scm_is_pair (s); s = scm_cdr (s))
                {
                  results.push_back (
                      format_single_argument (scm_car (s), precision));
                  if (!scm_is_null (scm_cdr (s)))
                    results.push_back (" ");
                }

              if (!scm_is_null (s))
                results.push_back (format_single_argument (s, precision));
            }
        }

      i = tilde;
    }

  if (scm_is_pair (rest))
    programming_error (string (__FUNCTION__) + ": too many arguments");

  vsize len = 0;
  for (vsize i = 0; i < results.size (); i++)
    len += results[i].size ();

  char *result = (char *)scm_malloc (len + 1);
  char *ptr = result;
  for (vsize i = 0; i < results.size (); i++)
    {
      // strcpy and strncpy cannot be used here
      // because std::string may contain '\0' in its contents.
      results[i].copy (ptr, results[i].size ());
      ptr += results[i].size ();
    }
  *ptr = '\0';

  return scm_take_locale_stringn (result, len);
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
      warning (_f ("g_spawn_sync failed (%d): %s: %s", exit_status, argv[0],
                   error->message));
      g_error_free (error);
      if (!exit_status)
        exit_status = -1;
    }

  return exit_status;
}

static char *
ly_scm2utf8 (SCM str)
{
  char *p = ly_scm2str0 (str);
  char *g = g_locale_to_utf8 (p, -1, 0, 0, 0);
  free (p);
  return g;
}

LY_DEFINE (ly_spawn, "ly:spawn", 1, 0, 1, (SCM command, SCM rest),
           "Simple interface to g_spawn_sync"
           " @var{str}."
           "  The error is formatted with @code{format} and @var{rest}.")

{
  LY_ASSERT_TYPE (scm_is_string, command, 1);

  long argc = scm_is_pair (rest) ? scm_ilength (rest) : 0;
  char **argv = new char *[argc + 2];

  int n = 0;
  argv[n++] = ly_scm2utf8 (command);
  for (SCM s = rest; scm_is_pair (s); s = scm_cdr (s))
    argv[n++] = ly_scm2utf8 (scm_car (s));
  argv[n] = 0;

  char *standard_output = 0;
  char *standard_error = 0;
  // Always get the pointer to the stdout/stderr messages
  int exit_status = ly_run_command (argv, &standard_output, &standard_error);

  if (standard_output && standard_error)
    {
      // Print out stdout and stderr only in debug mode
      debug_output (string ("\n") + standard_output + standard_error, true);
    }

  g_free (standard_error);
  g_free (standard_output);

  for (int i = 0; i < n; i++)
    free (argv[i]);
  delete[] argv;

  return scm_from_int (exit_status);
}
