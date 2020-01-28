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

#include "lily-guile.hh"

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring> /* strdup, strchr */

#include "dimensions.hh"
#include "direction.hh"
#include "file-path.hh"
#include "international.hh"
#include "libc-extension.hh"
#include "lily-imports.hh"
#include "main.hh"
#include "misc.hh"
#include "offset.hh"
#include "pitch.hh"
#include "source-file.hh"
#include "string-convert.hh"
#include "version.hh"
#include "warn.hh"

using std::string;
using std::vector;

/*
  symbols/strings.
 */
string
ly_scm_write_string (SCM s)
{
  SCM port
      = scm_mkstrport (SCM_INUM0, scm_make_string (SCM_INUM0, SCM_UNDEFINED),
                       SCM_OPN | SCM_WRTNG, "ly_write2string");
  scm_write (s, port);
  return ly_scm2string (scm_strport_to_string (port));
}

SCM
ly_quote_scm (SCM s)
{
  return scm_list_2 (ly_symbol2scm ("quote"), s);
}

string
ly_symbol2string (SCM s)
{
  /*
    Ugh. this is not very efficient.
  */
  return ly_scm2string (scm_symbol_to_string (s));
}

string
robust_symbol2string (SCM sym, const string &str)
{
  return scm_is_symbol (sym) ? ly_symbol2string (sym) : str;
}

string
gulp_file_to_string (const string &fn, bool must_exist, int size)
{
  string s = global_path.find (fn);
  if (s == "")
    {
      if (must_exist)
        {
          string e = _f ("cannot find file: `%s'", fn);
          e += " ";
          e += _f ("(load path: `%s')", global_path.to_string ());
          error (e);
          /* unreachable */
        }
      return s;
    }

  debug_output ("[" + s, true);

  vector<char> chars = gulp_file (s, size);
  string result (&chars[0], chars.size ());

  debug_output ("]\n", false);

  return result;
}

extern "C"
{
  // maybe gdb 5.0 becomes quicker if it doesn't do fancy C++ typing?
  void ly_display_scm (SCM s)
  {
    scm_display (s, scm_current_output_port ());
    scm_newline (scm_current_output_port ());
  }
};

/*
  STRINGS
 */
string
ly_scm2string (SCM str)
{
  assert (scm_is_string (str));
  string result;
  size_t len = scm_c_string_length (str);
  if (len)
    {
      result.resize (len);
      scm_to_locale_stringbuf (str, &result.at (0), len);
    }
  return result;
}

SCM
ly_string2scm (string const &str)
{
  return scm_from_locale_stringn (str.c_str (), str.length ());
}

char *
ly_scm2str0 (SCM str)
{
  return scm_to_utf8_string (str);
}

/*
  PAIRS
*/
SCM
index_get_cell (SCM s, Direction d)
{
  assert (d);
  return (d == LEFT) ? scm_car (s) : scm_cdr (s);
}

SCM
index_set_cell (SCM s, Direction d, SCM v)
{
  if (d == LEFT)
    scm_set_car_x (s, v);
  else if (d == RIGHT)
    scm_set_cdr_x (s, v);
  return s;
}

bool
is_number_pair (SCM p)
{
  return scm_is_pair (p) && scm_is_number (scm_car (p))
         && scm_is_number (scm_cdr (p));
}

unsigned int
ly_scm_hash (SCM s)
{
  return scm_ihashv (s, ~1u);
}

bool
is_axis (SCM s)
{
  if (scm_is_integer (s))
    {
      int i = scm_to_int (s);
      return i == 0 || i == 1;
    }
  return false;
}

bool
to_boolean (SCM s)
{
  return scm_is_bool (s) && ly_scm2bool (s);
}

/*
  DIRECTIONS
 */
Direction
to_dir (SCM s)
{
  return scm_is_integer (s) ? (Direction)scm_to_int (s) : CENTER;
}

Direction
robust_scm2dir (SCM d, Direction def)
{
  if (is_direction (d))
    def = to_dir (d);
  return def;
}

bool
is_direction (SCM s)
{
  if (scm_is_number (s))
    {
      int i = scm_to_int (s);
      return i >= -1 && i <= 1;
    }
  return false;
}

/*
  INTERVALS
 */
Interval
ly_scm2interval (SCM p)
{
  return Interval (scm_to_double (scm_car (p)), scm_to_double (scm_cdr (p)));
}

Drul_array<Real>
ly_scm2realdrul (SCM p)
{
  return Drul_array<Real> (scm_to_double (scm_car (p)),
                           scm_to_double (scm_cdr (p)));
}

SCM
ly_interval2scm (Drul_array<Real> i)
{
  return scm_cons (scm_from_double (i[LEFT]), scm_from_double (i[RIGHT]));
}

Interval
robust_scm2interval (SCM k, Drul_array<Real> v)
{
  Interval i;
  i[LEFT] = v[LEFT];
  i[RIGHT] = v[RIGHT];
  if (is_number_pair (k))
    i = ly_scm2interval (k);
  return i;
}

Drul_array<Real>
robust_scm2drul (SCM k, Drul_array<Real> v)
{
  if (is_number_pair (k))
    v = ly_scm2interval (k);
  return v;
}

Drul_array<bool>
robust_scm2booldrul (SCM k, Drul_array<bool> def)
{
  if (scm_is_pair (k))
    {
      def[LEFT] = to_boolean (scm_car (k));
      def[RIGHT] = to_boolean (scm_cdr (k));
    }
  return def;
}

/*
  OFFSET
*/
SCM
ly_offset2scm (Offset o)
{
  return scm_cons (scm_from_double (o[X_AXIS]), scm_from_double (o[Y_AXIS]));
}

Offset
ly_scm2offset (SCM s)
{
  return Offset (scm_to_double (scm_car (s)), scm_to_double (scm_cdr (s)));
}

Offset
robust_scm2offset (SCM k, Offset o)
{
  if (is_number_pair (k))
    o = ly_scm2offset (k);
  return o;
}
SCM
ly_offsets2scm (vector<Offset> os)
{
  SCM l = SCM_EOL;
  SCM *tail = &l;
  for (vsize i = 0; i < os.size (); i++)
    {
      *tail = scm_cons (ly_offset2scm (os[i]), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }
  return l;
}

vector<Offset>
ly_scm2offsets (SCM s)
{
  vector<Offset> os;
  for (; scm_is_pair (s); s = scm_cdr (s))
    os.push_back (ly_scm2offset (scm_car (s)));
  return os;
}

/*
  ALIST
*/
SCM
ly_alist_vals (SCM alist)
{
  SCM x = SCM_EOL;
  for (SCM p = alist; scm_is_pair (p); p = scm_cdr (p))
    x = scm_cons (scm_cdar (p), x);
  return x;
}

/*
  LISTS
 */

/* Return I-th element, or last elt L. If I < 0, then we take the first
   element.

   PRE: length (L) > 0  */
SCM
robust_list_ref (int i, SCM l)
{
  while (i-- > 0 && scm_is_pair (scm_cdr (l)))
    l = scm_cdr (l);
  return scm_car (l);
}

SCM
ly_deep_copy (SCM src)
{
  if (scm_is_pair (src))
    {
      SCM res = SCM_EOL;
      do
        {
          res = scm_cons (ly_deep_copy (scm_car (src)), res);
          src = scm_cdr (src);
        }
      while (scm_is_pair (src));
      // Oh, come on, GUILE.  Why do you require the second argument
      // of scm_reverse_x to be a proper list?  That makes no sense.
      // return scm_reverse_x (res, ly_deep_copy (src));
      SCM last_cons = res;
      res = scm_reverse_x (res, SCM_EOL);
      scm_set_cdr_x (last_cons, ly_deep_copy (src));
      return res;
    }
  if (scm_is_vector (src))
    {
      vsize len = scm_c_vector_length (src);
      SCM nv = scm_c_make_vector (len, SCM_UNDEFINED);
      for (vsize i = 0; i < len; i++)
        {
          scm_c_vector_set_x (nv, i, ly_deep_copy (scm_c_vector_ref (src, i)));
        }
      return nv;
    }
  return src;
}

string
print_scm_val (SCM val)
{
  string realval = ly_scm_write_string (val);
  if (realval.length () > 200)
    realval = realval.substr (0, 100) + "\n :\n :\n"
              + realval.substr (realval.length () - 100);
  return realval;
}

bool
type_check_assignment (SCM sym, SCM val, SCM type_symbol)
{

  // If undefined, some internal function caused it...should never happen.
  assert (!SCM_UNBNDP (val));
  if (!scm_is_symbol (sym))
    return false;

  SCM type = scm_object_property (sym, type_symbol);

  if (!scm_is_null (type) && !ly_is_procedure (type))
    {
      warning (_f ("cannot find property type-check for `%s' (%s).",
                   ly_symbol2string (sym).c_str (),
                   ly_symbol2string (type_symbol).c_str ())
               + "  " + _ ("perhaps a typing error?"));

      /* Be strict when being anal :) */
      if (do_internal_type_checking_global)
        scm_throw (ly_symbol2scm ("ly-file-failed"),
                   scm_list_3 (ly_symbol2scm ("typecheck"), sym, val));

      warning (_ ("skipping assignment"));
      return false;
    }

  /*
    Always succeeds.


    TODO: should remove #f from allowed vals?
  */
  if (scm_is_null (val) || scm_is_false (val))
    return true;

  if (!scm_is_null (val) && ly_is_procedure (type)
      && scm_is_false (scm_call_1 (type, val)))
    {
      SCM type_name = Lily::type_name (type);

      warning (
          _f ("type check for `%s' failed; value `%s' must be of type `%s'",
              ly_symbol2string (sym).c_str (), print_scm_val (val),
              ly_scm2string (type_name).c_str ()));
      progress_indication ("\n");
      return false;
    }
  return true;
}

void
ly_wrong_smob_arg (bool pred (SCM), SCM var, int number, const char *fun)
{
  string type = predicate_to_typename ((void *)pred);
  if (pred (var))
    {
      // Uh oh.  unsmob<T> delivered 0, yet
      // unsmob<T> delivers true.  This means that unsmob<T> is a
      // matching check from a base class of T, but var is of an
      // incompatible derived type.
      type = string (_ ("Wrong kind of ")).append (type);
    }
  scm_wrong_type_arg_msg (mangle_cxx_identifier (fun).c_str (), number, var,
                          type.c_str ());
}

/* some SCM abbrevs

zijn deze nou handig?
zijn ze er al in scheme, maar heten ze anders? */

/* Remove doubles from (sorted) list */
SCM
ly_unique (SCM list)
{
  SCM unique = SCM_EOL;
  for (SCM i = list; scm_is_pair (i); i = scm_cdr (i))
    {
      if (!scm_is_pair (scm_cdr (i))
          || !ly_is_equal (scm_car (i), scm_cadr (i)))
        unique = scm_cons (scm_car (i), unique);
    }
  return scm_reverse_x (unique, SCM_EOL);
}

/* Split list at member s, removing s.
   Return (BEFORE . AFTER)  */
SCM
ly_split_list (SCM s, SCM list)
{
  SCM before = SCM_EOL;
  SCM after = list;
  for (; scm_is_pair (after);)
    {
      SCM i = scm_car (after);
      after = scm_cdr (after);
      if (ly_is_equal (i, s))
        break;
      before = scm_cons (i, before);
    }
  return scm_cons (scm_reverse_x (before, SCM_EOL), after);
}

void
taint (SCM *)
{
  /*
    nop.
  */
}

/*
  display stuff without using stack
*/
SCM
display_list (SCM s)
{
  SCM p = scm_current_output_port ();

  scm_puts ("(", p);
  for (; scm_is_pair (s); s = scm_cdr (s))
    {
      scm_display (scm_car (s), p);
      scm_puts (" ", p);
    }
  scm_puts (")", p);
  return SCM_UNSPECIFIED;
}

// Needed as complement to int_list_to_slice since scm_c_memq refuses
// to work with dotted lists.

SCM
ly_memv (SCM v, SCM l)
{
  for (; scm_is_pair (l); l = scm_cdr (l))
    if (scm_is_true (scm_eqv_p (v, scm_car (l))))
      return l;
  return SCM_BOOL_F;
}

Slice
int_list_to_slice (SCM l)
{
  Slice s;
  s.set_empty ();
  for (; scm_is_pair (l); l = scm_cdr (l))
    if (scm_is_number (scm_car (l)))
      s.add_point (scm_to_int (scm_car (l)));
  return s;
}

Real
robust_scm2double (SCM k, double x)
{
  if (scm_is_number (k))
    x = scm_to_double (k);
  return x;
}

vector<Real>
ly_scm2floatvector (SCM l)
{
  vector<Real> floats;
  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    floats.push_back (robust_scm2double (scm_car (s), 0.0));
  return floats;
}

SCM
ly_floatvector2scm (vector<Real> v)
{
  SCM l = SCM_EOL;
  SCM *tail = &l;
  for (vsize i = 0; i < v.size (); i++)
    {
      *tail = scm_cons (scm_from_double (v[i]), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }
  return l;
}

string
robust_scm2string (SCM k, const string &s)
{
  if (scm_is_string (k))
    return ly_scm2string (k);
  return s;
}

int
robust_scm2int (SCM k, int o)
{
  if (scm_is_integer (k))
    o = scm_to_int (k);
  return o;
}

vsize
robust_scm2vsize (SCM k, vsize o)
{
  if (scm_is_integer (k))
    {
      int i = scm_to_int (k);
      if (i >= 0)
        return (vsize)i;
    }
  return o;
}

SCM
ly_rational2scm (Rational r)
{
  if (r.is_infinity ())
    {
      if (r > Rational (0))
        return scm_inf ();

      return scm_difference (scm_inf (), SCM_UNDEFINED);
    }

  return scm_divide (scm_from_int64 (r.numerator ()),
                     scm_from_int64 (r.denominator ()));
}

Rational
ly_scm2rational (SCM r)
{
  if (scm_is_true (scm_inf_p (r)))
    {
      if (scm_is_true (scm_positive_p (r)))
        {
          Rational r;
          r.set_infinite (1);
          return r;
        }
      else
        {
          Rational r;
          r.set_infinite (-1);
          return r;
        }
    }

  return Rational (scm_to_int64 (scm_numerator (r)),
                   scm_to_int64 (scm_denominator (r)));
}

Rational
robust_scm2rational (SCM n, Rational rat)
{
  if (ly_is_rational (n))
    return ly_scm2rational (n);
  else
    return rat;
}

bool
ly_is_rational (SCM n)
{
  return (scm_is_real (n)
          && (scm_is_true (scm_exact_p (n)) || scm_is_true (scm_inf_p (n))));
}

SCM
alist_to_hashq (SCM alist)
{
  long i = scm_ilength (alist);
  if (i < 0)
    return scm_c_make_hash_table (0);

  SCM tab = scm_c_make_hash_table (i);
  for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM pt = scm_cdar (s);
      scm_hashq_set_x (tab, scm_caar (s), pt);
    }
  return tab;
}

SCM
ly_hash2alist (SCM tab)
{
  return Lily::hash_table_to_alist (tab);
}

/*
  C++ interfacing.
 */

string
mangle_cxx_identifier (string cxx_id)
{
  if (cxx_id.substr (0, 3) == "ly_")
    cxx_id = cxx_id.replace (0, 3, "ly:");
  else
    {
      cxx_id = String_convert::to_lower (cxx_id);
      cxx_id = "ly:" + cxx_id;
    }
  if (cxx_id.substr (cxx_id.length () - 2) == "_p")
    cxx_id = cxx_id.replace (cxx_id.length () - 2, 2, "?");
  else if (cxx_id.substr (cxx_id.length () - 2) == "_x")
    cxx_id = cxx_id.replace (cxx_id.length () - 2, 2, "!");

  replace_all (&cxx_id, "_less?", "<?");
  replace_all (&cxx_id, "_2_", "->");
  replace_all (&cxx_id, "__", "::");
  replace_all (&cxx_id, '_', '-');

  return cxx_id;
}

SCM
ly_string_array_to_scm (vector<string> a)
{
  SCM s = SCM_EOL;
  for (vsize i = a.size (); i; i--)
    s = scm_cons (ly_symbol2scm (a[i - 1].c_str ()), s);
  return s;
}

/* SYMBOLS is a whitespace separated list.  */
SCM
parse_symbol_list (char const *symbols)
{
  while (isspace (*symbols))
    symbols++;
  string s = symbols;
  replace_all (&s, '\n', ' ');
  replace_all (&s, '\t', ' ');
  replace_all (&s, "  ", " ");
  return ly_string_array_to_scm (string_split (s, ' '));
}

/* GDB debugging. */
struct ly_t_double_cell
{
  SCM a;
  SCM b;
  SCM c;
  SCM d;
};

/* inserts at front, removing duplicates */
SCM
ly_assoc_prepend_x (SCM alist, SCM key, SCM val)
{
  return scm_acons (key, val, scm_assoc_remove_x (alist, key));
}
