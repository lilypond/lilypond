/*
  lily-guile.cc -- implement assorted Guile bindings

  source file of the GNU LilyPond music typesetter

  (c) 1998--2007 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "config.hh"

#include <cstdio>
#include <cstring>  /* memset */
using namespace std;

#include "international.hh"
#include "libc-extension.hh"
#include "lily-guile.hh"
#include "std-string.hh"
#include "misc.hh"
#include "warn.hh"
#include "version.hh"
#include "dimensions.hh"
#include "main.hh"
#include "file-path.hh"

LY_DEFINE (ly_find_file, "ly:find-file",
	   1, 0, 0, (SCM name),
	   "Return the absolute file name of @var{name}, "
	   "or @code{#f} if not found.")
{
  SCM_ASSERT_TYPE (scm_is_string (name), name, SCM_ARG1, __FUNCTION__, "string");

  string nm = ly_scm2string (name);
  string file_name = global_path.find (nm);
  if (file_name.empty ())
    return SCM_BOOL_F;

  return scm_makfrom0str (file_name.c_str ());
}

/*
  Ugh. Gulped file is copied twice. (maybe thrice if you count stdio
  buffering.)
*/
LY_DEFINE (ly_gulp_file, "ly:gulp-file",
	   1, 1, 0, (SCM name, SCM size),
	   "Read the file @var{name}, and return its contents in a string.  "
	   "The file is looked up using the search path. ")
{
  SCM_ASSERT_TYPE (scm_is_string (name), name, SCM_ARG1, __FUNCTION__, "string");
  int sz = INT_MAX;
  if (size != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE (scm_is_number (size), size, SCM_ARG2, __FUNCTION__, "number");
      sz = scm_to_int (size);
    }
  
  string contents = gulp_file_to_string (ly_scm2string (name), true, sz);
  return scm_from_locale_stringn (contents.c_str (), contents.length ());
}

LY_DEFINE (ly_error, "ly:error",
	   1, 0, 1, (SCM str, SCM rest),
	   "Scheme callable function to issue the error @code{msg}. "
	   "The error is formatted with @code{format} and @code{rest}.")
{
  SCM_ASSERT_TYPE (scm_is_string (str), str, SCM_ARG1, __FUNCTION__, "string");
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  error (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_message, "ly:message",
	   1, 0, 1, (SCM str, SCM rest),
	   "Scheme callable function to issue the message @code{msg}. "
	   "The message is formatted with @code{format} and @code{rest}.")
{
  SCM_ASSERT_TYPE (scm_is_string (str), str, SCM_ARG1, __FUNCTION__, "string");
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  message (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_progress, "ly:progress",
	   1, 0, 1, (SCM str, SCM rest),
	   "Scheme callable function to print progress @code{str}. "
	   "The message is formatted with @code{format} and @code{rest}.")
{
  SCM_ASSERT_TYPE (scm_is_string (str), str, SCM_ARG1, __FUNCTION__, "string");
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  progress_indication (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_programming_error, "ly:programming-error",
	   1, 0, 1, (SCM str, SCM rest),
	   "Scheme callable function to issue the warning @code{msg}. "
	   "The message is formatted with @code{format} and @code{rest}.")
{
  SCM_ASSERT_TYPE (scm_is_string (str), str, SCM_ARG1, __FUNCTION__, "string");
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  programming_error (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_warning, "ly:warning",
	   1, 0, 1, (SCM str, SCM rest),
	   "Scheme callable function to issue the warning @code{str}. "
	   "The message is formatted with @code{format} and @code{rest}.")
{
  SCM_ASSERT_TYPE (scm_is_string (str), str, SCM_ARG1, __FUNCTION__, "string");
  str = scm_simple_format (SCM_BOOL_F, str, rest);
  warning (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_dir_p, "ly:dir?",
	   1, 0, 0, (SCM s),
	   "type predicate. A direction is @code{-1}, @code{0} or "
	   "@code{1}, where @code{-1} represents "
	   "left or down and @code{1} represents right or up.")
{
  if (scm_is_number (s))
    {
      int i = scm_to_int (s);
      return (i >= -1 && i <= 1) ? SCM_BOOL_T : SCM_BOOL_F;
    }
  return SCM_BOOL_F;
}

LY_DEFINE (ly_assoc_get, "ly:assoc-get",
	   2, 1, 0,
	   (SCM key, SCM alist, SCM default_value),
	   "Return value if KEY in ALIST, else DEFAULT-VALUE "
	   "(or #f if not specified).")
{
  SCM handle = scm_assoc (key, alist);

  if (default_value == SCM_UNDEFINED)
    default_value = SCM_BOOL_F;

  if (scm_is_pair (handle))
    return scm_cdr (handle);
  return default_value;
}

LY_DEFINE (ly_string_substitute, "ly:string-substitute",
	   3, 0, 0, (SCM a, SCM b, SCM s),
	   "Replace @var{a} by @var{b} in @var{s}.")
{
  SCM_ASSERT_TYPE (scm_is_string (a), s, SCM_ARG1, __FUNCTION__, "string");
  SCM_ASSERT_TYPE (scm_is_string (b), s, SCM_ARG2, __FUNCTION__, "string");
  SCM_ASSERT_TYPE (scm_is_string (s), s, SCM_ARG3, __FUNCTION__, "string");

  string ss = ly_scm2string (s);
  replace_all (ss, string (scm_i_string_chars (a)),
		   string (scm_i_string_chars (b)));
  return ly_string2scm (ss);
}
  
LY_DEFINE (ly_number2string, "ly:number->string",
	   1, 0, 0, (SCM s),
	   "Convert @var{num} to a string without generating many decimals.")
{
  SCM_ASSERT_TYPE (scm_is_number (s), s, SCM_ARG1, __FUNCTION__, "number");

  char str[400];			// ugh.

  if (scm_exact_p (s) == SCM_BOOL_F)
    {
      Real r (scm_to_double (s));
	if (isinf (r) || isnan (r))
	  {
	    programming_error (_ ("infinity or NaN encountered while converting Real number"));
	    programming_error (_ ("setting to zero"));

	    r = 0.0;
	  }

      snprintf (str, sizeof (str), "%.4f", r);
    }
  else
    snprintf (str, sizeof (str), "%d", int (scm_to_int (s)));

  return scm_makfrom0str (str);
}

LY_DEFINE (ly_version, "ly:version", 0, 0, 0, (),
	   "Return the current lilypond version as a list, e.g. @code{(1 3 127 uu1)}. ")
{
  char const *vs = "\'(" MAJOR_VERSION " " MINOR_VERSION " " PATCH_LEVEL " " MY_PATCH_LEVEL ")";

  return scm_c_eval_string ((char *)vs);
}

LY_DEFINE (ly_unit, "ly:unit", 0, 0, 0, (),
	   "Return the unit used for lengths as a string.")
{
  return scm_makfrom0str (INTERNAL_UNIT);
}

LY_DEFINE (ly_dimension_p, "ly:dimension?", 1, 0, 0, (SCM d),
	   "Return @var{d} is a number. Used to distinguish length "
	   "variables from normal numbers.")
{
  return scm_number_p (d);
}

/*
  Debugging mem leaks:
*/
LY_DEFINE (ly_protects, "ly:protects",
	   0, 0, 0, (),
	   "Return hash of protected objects.")
{
  return scm_protects;
}

LY_DEFINE (ly_gettext, "ly:gettext",
	   1, 0, 0, (SCM string),
	   "Gettext wrapper.")
{
  SCM_ASSERT_TYPE (scm_is_string (string), string, SCM_ARG1,
		   __FUNCTION__, "string");
  return scm_makfrom0str (_ (scm_i_string_chars (string)).c_str ());
}

LY_DEFINE (ly_output_backend, "ly:output-backend",
	   0, 0, 0, (),
	   "Return name of output backend.")
{
  return scm_makfrom0str (output_backend_global.c_str ());
}

LY_DEFINE (ly_output_formats, "ly:output-formats",
	   0, 0, 0, (),
	   "Formats passed to --format as a list of strings, "
	   "used for the output.")
{
  vector<string> output_formats = string_split (output_format_global, ',');

  SCM lst = SCM_EOL;
  int output_formats_count = output_formats.size ();
  for (int i = 0; i < output_formats_count; i++)
    lst = scm_cons (scm_makfrom0str (output_formats[i].c_str ()), lst);

  return lst;
}

LY_DEFINE (ly_wchar_to_utf_8, "ly:wide-char->utf-8",
	   1, 0, 0, (SCM wc),
	   "Encode the Unicode codepoint @var{wc}, an integer, as UTF-8")
{
  char buf[5];

  SCM_ASSERT_TYPE (scm_is_integer (wc), wc, SCM_ARG1, __FUNCTION__, "integer");
  unsigned wide_char = (unsigned) scm_to_int (wc);
  char *p = buf;

  if (wide_char < 0x0080)
    *p++ = (char)wide_char;
  else if (wide_char < 0x0800)
    {
      *p++ = (char) (((wide_char >> 6)) | 0xC0);
      *p++ = (char) (((wide_char) & 0x3F) | 0x80);
    }
  else if (wide_char < 0x10000)
    {
      *p++ = (char) (((wide_char >> 12)) | 0xE0);
      *p++ = (char) (((wide_char >> 6) & 0x3F) | 0x80);
      *p++ = (char) (((wide_char) & 0x3F) | 0x80);
    }
  else
    {
      *p++ = (char) (((wide_char >> 18)) | 0xF0);
      *p++ = (char) (((wide_char >> 12) & 0x3F) | 0x80);
      *p++ = (char) (((wide_char >> 6) & 0x3F) | 0x80);
      *p++ = (char) (((wide_char) & 0x3F) | 0x80);
    }
  *p = 0;

  return scm_makfrom0str (buf);
}

LY_DEFINE (ly_effective_prefix, "ly:effective-prefix",
	   0, 0, 0, (),
	   "Return effective prefix.")
{
  return scm_makfrom0str (lilypond_datadir.c_str ());
}

LY_DEFINE (ly_chain_assoc_get, "ly:chain-assoc-get",
	   2, 1, 0, (SCM key, SCM achain, SCM dfault),
	   "Return value for @var{key} from a list of alists @var{achain}.  "
	   "If no if no entry is found, return DFAULT, "
	   "or #f if no DFAULT not specified.")
{
  if (scm_is_pair (achain))
    {
      SCM handle = scm_assoc (key, scm_car (achain));
      if (scm_is_pair (handle))
	return scm_cdr (handle);
      else
	return ly_chain_assoc_get (key, scm_cdr (achain), dfault);
    }
  return dfault == SCM_UNDEFINED ? SCM_BOOL_F : dfault;
}

LY_DEFINE (ly_stderr_redirect, "ly:stderr-redirect",
	   1, 1, 0, (SCM file_name, SCM mode),
	   "Redirect stderr to FILE-NAME, opened with MODE.")
{
  SCM_ASSERT_TYPE (scm_is_string (file_name), file_name, SCM_ARG1,
		   __FUNCTION__, "file_name");
  char const *m = "w";
  if (mode != SCM_UNDEFINED && scm_string_p (mode))
    m = ly_scm2newstr (mode, 0);
  /* dup2 and (fileno (current-error-port)) do not work with mingw'c
     gcc -mwindows.  */
  freopen (ly_scm2newstr (file_name, 0), m, stderr);
  return SCM_UNSPECIFIED;
}

static SCM
accumulate_symbol (void *closure, SCM key, SCM val, SCM result)
{
  (void) closure;
  (void) val;
  return scm_cons (key, result);
}

LY_DEFINE(ly_hash_table_keys, "ly:hash-table-keys",
	  1,0,0, (SCM tab),
	  "return a list of keys in @var{tab}")
{
  return scm_internal_hash_fold ((Hash_closure_function) & accumulate_symbol,
				 NULL, SCM_EOL, tab);
}

LY_DEFINE (ly_camel_case_to_lisp_identifier, "ly:camel-case->lisp-identifier",
	   1, 0, 0, (SCM name_sym),
	   "Convert FooBar to foo-bar style symbol.")
{
  SCM_ASSERT_TYPE(scm_is_symbol (name_sym), name_sym,
		  SCM_ARG1, __FUNCTION__, "symbol");
  
  /*
    TODO: should use strings instead?
  */
  
  const string in = ly_symbol2string (name_sym);
  
  vector<char>  out;

  /* don't add '-' before first character */
  out.push_back (tolower (in[0]));
    
  for (size_t inpos = 1; inpos < in.size (); inpos++)
    {
      if (isupper (in[inpos]))
	out.push_back ('-');
      out.push_back (tolower (in[inpos]));
    }

  string result (&out[0], out.size ());
  return ly_symbol2scm (result.c_str ());
}
